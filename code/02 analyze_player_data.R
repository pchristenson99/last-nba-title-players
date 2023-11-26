################################################################################
### Title:  Analyze player season, team data
### Author: Peter Christenson
################################################################################

rm(list = ls()); gc()
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  lubridate,
  magrittr,
  readxl,
  writexl,
  survival,
  zoo,
  tidyr,
  stringr,
  janitor,
  readr,
  data.table,
  httr,
  XML
)

##################################################
###  Globals
##################################################

# Directories
WORKING_DIRECTORY <- "SET WD HERE"
INPUT <- "./input/"
INTERMEDIATE <- "./intermediate/"
OUTPUT <- "./output/"
setwd(WORKING_DIRECTORY)

# Constants
DATE <- Sys.Date()
PULL_WIKIPEDIA_PAGES <- F # Switch to call Wikipedia URLs and scrape tables
                          # (if set to F, will use version already saved down)

##################################################
###  Data
##################################################

# Player season, team data
player_data_raw <- read_csv(
  paste0(INTERMEDIATE, "players_seasons_teams.csv")
) %>% 
  clean_names() %>% 
  select(-x1)

##################################################
###  Functions
##################################################

# Function to recursively link team relocations
link.relo <- function(team_find) {
  relo_filt <- nba_relocations %>% 
    filter(team == team_find) %>% 
    pull(relocated_to)
  if (relo_filt %in% nba_relocations$team) {
    recursive_results <- link.relo(relo_filt)
    return(c(relo_filt, recursive_results))  
  }
  return(relo_filt)
}

# Function to clean linked team relocations
clean.relo.links <- function(team_find) {
  # Create full list of team names, with last team being final destination
  relos <- link.relo(team_find)
  relos <- append(team_find, relos)
  final_team  <- tail(relos, n=1)

  # Create dataframe with all teams
  df <- data.frame(relos) %>% 
    rename(other = 1) %>% 
    mutate(master_team = final_team) %>% 
    select(master_team, other)
}

##################################################
###  Clean data
##################################################

#----- Create clean player dataset -----#
player_data_cleaned <- player_data_raw %>% 
  filter(status == "OK") %>% # Errors are for players without any teams
  mutate(
    season = str_sub(team_year, -7, -4), # Use season start year as season
    team   = str_sub(team_year, 1, -9),
    team   = gsub("—|–", "-", team)
  ) %>% 
  select(index, player = name, season, team)

# Check years, teams
View(as.data.frame(unique(player_data_cleaned$season))) # All seasons are valid
View(as.data.frame(unique(player_data_cleaned$team))) # Needs some cleaning

player_data_cleaned %<>%
  mutate(team = recode(team,
                       "Denver Nuggets(o)" = "Denver Nuggets",
                       "New Orleans-Oklahoma City Hornets" = "New Orleans Hornets"
  ))
rm(player_data_raw); gc()

#----- List of NBA champions -----#
if (PULL_WIKIPEDIA_PAGES) {
  url <- "https://en.wikipedia.org/wiki/List_of_NBA_champions"
  request <- GET(url)
  tables <- readHTMLTable(doc=content(request, "text"))
  nba_champions <- as.data.frame(tables[2])
  saveRDS(
    nba_champions,
    paste0(INTERMEDIATE, "wikipedia_nba_champions.rds")
  )
  rm(url, request, tables); gc()
} else {
  nba_champions <- readRDS(paste0(INTERMEDIATE, "wikipedia_nba_champions.rds"))
}

nba_champions %<>% 
  row_to_names(row_number = 1) %>%
  clean_names() %>% 
  mutate(year = str_sub(year, 1, 4)) %>%
  filter(!is.na(as.numeric(year))) %>% 
  mutate(across(contains("champion"), ~gsub(" \\(.*", "", .))) %>% 
  mutate(
    season   = as.character(as.numeric(year) - 1), # Year refers to actual year of championship
    champion = ifelse(
      as.numeric(str_sub(result, 1, 1)) > as.numeric(str_sub(result, 3, 3)),
      western_champion,
      eastern_champion
    ),
    champion = gsub("[^a-zA-Z0-9 .]", "", champion)
  ) %>% 
  select(season, team = champion)

##################################################
###  Handle team relocations, name changes
##################################################

#----- Pull data -----#
if (PULL_WIKIPEDIA_PAGES) {
  url <- "https://en.wikipedia.org/wiki/List_of_relocated_National_Basketball_Association_teams"
  request <- GET(url)
  tables <- readHTMLTable(doc=content(request, "text"))
  nba_relocations <- as.data.frame(tables[2])
  saveRDS(
    nba_relocations,
    paste0(INTERMEDIATE, "wikipedia_nba_relocations.rds")
  )
  rm(url, request, tables); gc()
} else {
  nba_relocations <- readRDS(paste0(INTERMEDIATE, "wikipedia_nba_relocations.rds"))
}

nba_relocations %<>% 
  row_to_names(row_number = 1) %>%
  clean_names() %>% 
  mutate(across(c(team, relocated_to), ~gsub("\\^|\\*|\\+", "", .)),
         relocated_to = ifelse(grepl("Capital Bullets", relocated_to), "Capital Bullets", relocated_to)) %>% 
  select(team, relocated_to) %>% 
  # Ignore Toronto (was COVID related)
  filter(team != "Toronto Raptors")

#----- Create master dataset of all team names for relocations -----#

# Get all original teams
original_teams <- nba_relocations %>% # All original teams
  filter(!(team %in% nba_relocations$relocated_to)) %>% 
  filter(!grepl("hornets", tolower(team))) %>%  # Handle Hornets separately
  pull(team)

# Create dataframe for relocations
relo_df <- lapply(original_teams, clean.relo.links) %>% 
  bind_rows()

# Clean
relo_df %<>%
  mutate(
    master_team = recode(
      master_team,
      "San Francisco Warriors" = "Golden State Warriors",
      "Capital Bullets"        = "Washington Wizards"
    ),
  ) %>%
  mutate_all(~gsub("—|–", "-", .)) %>% 
  rbind(
    # Name changes
    tribble(~master_team, ~other,
            "Golden State Warriors", "Golden State Warriors",
            "Washington Wizards", "Washington Bullets",
            "Washington Wizards", "Chicago Packers",
            "Washington Wizards", "Washington Wizards",
            "Charlotte Hornets", "Charlotte Hornets",
            "Charlotte Hornets", "Charlotte Bobcats",
            "New Orleans Pelicans", "New Orleans Pelicans",
            "New Orleans Pelicans", "New Orleans Hornets")
  ) %>% 
  arrange(master_team, other)

#----- Add non-relocation teams -----#
non_relo_teams <- player_data_cleaned %>% 
  distinct(team) %>% 
  filter(!(team %in% relo_df$other)) %>% 
  arrange(team) %>% 
  mutate(master_team = team, other = team) %>% 
  select(master_team, other)

#----- Clean and combine both -----#
full_team_list <- rbind(relo_df, non_relo_teams) %>% 
  arrange(master_team, other)

#----- Check if any teams don't exist in player, champion dataframe -----#
missing_team_check_player <- full_team_list %>% 
  filter(!(other %in% player_data_cleaned$team)) # Only one team, no issue

missing_team_check_champ <- nba_champions %>% 
  filter(!(team %in% full_team_list$other)) # All teams are in dataset

##################################################
###  Identify last player on every championship
###  team
##################################################

#----- Clean team names, add players of each championship roster -----#

# Players with team name cleaned
players_team_cleaned <- player_data_cleaned %>% 
  left_join(full_team_list, by = c("team" = "other")) %>% 
  select(index, player, master_team, season) %>% 
  # Add count of teams (for dropping players who switched teams later)
  group_by(index) %>% 
  mutate(num_teams = cumsum(!duplicated(master_team))) %>% 
  ungroup()

championship_rosters <- nba_champions %>% 
  left_join(full_team_list, by = c("team" = "other")) %>% 
  left_join(players_team_cleaned %>% 
              select(-num_teams)) %>% 
  rename_all(~paste0("champion_", .))

#----- Add on rosters of championship teams in each year after -----#
championship_rosters %<>%
  left_join(players_team_cleaned,
            by = c("champion_master_team" = "master_team"),
            relationship = "many-to-many") %>% 
  arrange(champion_season, season) %>% 
  filter(as.numeric(season) >= as.numeric(champion_season))

rm(players_team_cleaned); gc()

#----- Identify last players from each team -----#

# Continuous version
last_championship_players_cont <- championship_rosters %>% 
  mutate(champ_player_on_team = ifelse(champion_index == index, 1, 0)) %>% 
  filter(champ_player_on_team == 1) %>% 
  # Ignore players who left team then returned
  mutate(season = as.numeric(season)) %>% 
  group_by(champion_season, champion_index) %>% 
  mutate(team_change     = num_teams - lag(num_teams),
         team_change     = if_else(is.na(team_change), 0, team_change),
         team_change_cum = cummax(team_change)) %>% 
  ungroup() %>% 
  filter(team_change_cum == 0) %>% 
  group_by(champion_season, champion_team) %>% 
  slice_max(as.numeric(season)) %>% 
  mutate(player_season = paste0(player, " (", season, ")")) %>% 
  group_by(champion_season, champion_team) %>% 
  summarize(last_player_season_continuous = paste(player_season, collapse = ", ")) %>% 
  ungroup() %>% 
  filter(champion_season <= 2021) # Only consider champions through 2021-22 season

# Non-continuous version
last_championship_players_non_cont <- championship_rosters %>% 
  mutate(champ_player_on_team = ifelse(champion_index == index, 1, 0)) %>% 
  filter(champ_player_on_team == 1) %>% 
  # Ignore players who left team then returned
  mutate(season = as.numeric(season)) %>% 
  group_by(champion_season, champion_team) %>% 
  slice_max(as.numeric(season)) %>% 
  mutate(player_season = paste0(player, " (", season, ")")) %>% 
  group_by(champion_season, champion_team) %>% 
  summarize(last_player_season_non_continuous = paste(player_season, collapse = ", ")) %>% 
  ungroup() %>% 
  filter(champion_season <= 2021) # Only consider champions through 2021-22 season

# Combine
last_championship_players <- full_join(
  last_championship_players_cont,
  last_championship_players_non_cont
)

#----- Export -----#
write_csv(
  last_championship_players,
  paste0(OUTPUT, "last_championship_players.csv")
)
