# Create performance chart

create_performance_chart <- function(results) {
  
  # firstly, get all results
  # as when needed to create league table
  home_results <- count_wins(results, home_team = TRUE)
  away_results <- count_wins(results, home_team = FALSE)
  all_results <- bind_rows(home_results, away_results)
  max_round <- all_results$Round %>% max
  
  chart_data <- tibble()
  top_teams <- c()
  for (i in 1:max_round) {
    round_results <- all_results %>% 
      filter(Round <= i)
    r <- paste0("Round",i)
    table0 <- create_league_table(round_results)
    table <- table0 %>% 
      mutate(pos = row_number()) %>% 
      rename(Round = P) %>% 
      select(Team, pos, Round)
    top_teams <- table %>% head(3) %>% pull(Team)
    if (nrow(chart_data) == 0) {
      chart_data <- table
    } else{
      chart_data <- bind_rows(chart_data, table)
    }
  }

  n_teams <- results %>% count(Team.1) %>% nrow()
  chart_data$pos <- n_teams+1 - chart_data$pos
  chart_data$Team <- as.factor(chart_data$Team)
  tcolors <- c("red","skyblue","royalblue")
  names(tcolors) <- top_teams
  
  chart_data %>%
    filter(Team %in% top_teams) %>% 
    mutate(Team = as.factor(Team)) %>% 
    ggplot(aes(x=Round, y=pos, color=Team)) +
    scale_colour_manual(values=tcolors) +
    geom_line() +
    geom_point(shape=21, size=1) +
    ggtitle("Performance Chart")
}

create_performance_chart(results)
