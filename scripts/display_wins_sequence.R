# Display wins sequence

get_wins_sequence <- function(results, one_column=TRUE) {
  
  # firstly, get all results
  # as when needed to create league table
  home_results <- count_wins(results, home_team = TRUE)
  away_results <- count_wins(results, home_team = FALSE)
  all_results <- bind_rows(home_results, away_results)
  
  # determine form (W/D/L) per round
  # and then widen the data horizontally
  forms <- all_results %>% 
    mutate(
      Form = case_when(
        n_win == 1 ~ "W",
        n_draw == 1 ~ "D",
        n_lose == 1 ~ "L",
        TRUE ~ "-"
      )
    ) %>%
    pivot_wider(
      id_cols = Team,
      names_prefix = "R",
      names_from = Round,
      values_from = Form
    ) %>% 
    arrange(Team)
  
  if (one_column) {
    # columns to paste together
    cols <- names(forms %>% select(-Team))
    
    # create a new column with the columns collapsed together
    forms$Form <- apply( forms[ , cols ] , 1 , paste , collapse = " " )
    
    return(forms %>% select(Team, Form))
  }
  return(forms)
}

forms <- get_wins_sequence(results, one_column = FALSE)

n_round <- length(names(forms))-1
col_rounds <- 2:(n_round+1)
round_color <- function () {
  formatter("span",
            style = function(x) style(`color` = ifelse(x == "W", "green", 
                                                       ifelse(x == "L", "red", "grey"))))
}

forms %>% formattable(
  list(
    area(col = col_rounds) ~ round_color()
  )
)
