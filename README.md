# last-nba-title-players
A project to identify the last player on each NBA champion.

I was interested in seeing who the last player to play for each NBA champion (as in, who from each title-winning team stayed on that team the longest in the seasons following their championship) was, starting with the first championship in the 1946-47 season (in what was then the Basketball Association of America) through the 2021-22 season. I consider two methodologies for evaluating the "last" player on each champion:
* The last player to the play for the team in a _continuous_ stint (i.e., only the first stint of players who started with the team, left for another, and later rejoined is considered)
* The last player to play for the team, regardless of whether they left and later rejoined the team

## Data Sources
[Hispanos NBA](https://en.hispanosnba.com)
* A website containing profile pages for current and historical basketball players
> [!NOTE]
> While I could not find any explicit statement that scraping from this website is a violation, I advise that any scraping done on Hispanos NBA or with my code be done responsibly and with caution. Additionally, I do not take responsibility for actions performed with any web scraping code I have written.

[List of NBA Champions](https://en.wikipedia.org/wiki/List_of_NBA_champions)
* A Wikipedia page containing a list of all NBA champions by season

[List of Relocated NBA Teams](https://en.wikipedia.org/wiki/List_of_relocated_National_Basketball_Association_teams)
* A Wikipedia page containing a list of all relocated NBA teams, as a team's relocation should not be considered when assessing whether a player is still on a championship team
## Analysis Structure
### Code
