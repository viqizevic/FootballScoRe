# Create league table

count_wins <- function(results, home_team = TRUE) {
  if (home_team) {
    team_results <- results %>% 
      mutate(
        Team = Team.1,
        goals_scored = Homegoal,
        goals_conceded = Awaygoal
      )
  } else {
    team_results <- results %>% 
      mutate(
        Team = Team.2,
        goals_scored = Awaygoal,
        goals_conceded = Homegoal
      )
  }
  WIN <- ifelse(home_team, "Home", "Away")
  LOSE <- ifelse(home_team, "Away", "Home")
  team_results %>% 
    mutate(
      n_play = 1,
      n_win  = ifelse(Winner==WIN, 1, 0),
      n_lose = ifelse(Winner==LOSE, 1, 0),
      n_draw = ifelse(Winner=="Draw", 1, 0)
    )
}

create_league_table <- function(team_results) {
  team_results %>% 
    group_by(Team) %>% 
    summarise(
      P = sum(n_play),
      W = sum(n_win),
      D = sum(n_draw),
      L = sum(n_lose),
      goals_scored = sum(goals_scored),
      goals_conceded = sum(goals_conceded)
    ) %>% 
    ungroup() %>%
    mutate(
      Goals = paste(goals_scored, goals_conceded, sep = ":"),
      Pts = 3 * W + D
    ) %>% 
    select(-matches("goals_")) %>% 
    arrange(-Pts)
}

home_results <- count_wins(results, home_team = TRUE)
away_results <- count_wins(results, home_team = FALSE)
all_results <- bind_rows(home_results, away_results)

home_table <- create_league_table(home_results)
away_table <- create_league_table(away_results)
all_table <- create_league_table(all_results)

home_table %>% formattable()
away_table %>% formattable()
all_table %>% formattable()
