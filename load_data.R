# Load data

csvpath <- "https://raw.githubusercontent.com/footballcsv/england/master/2010s/2019-20/eng.1.csv"
results_init <- read.csv(csvpath)

# get date in right format
results <- results_init %>% 
  mutate(
    Day = as.Date(Date, "%a %b %d %Y"),
    Homegoal = as.numeric(gsub("-\\d+", "", FT)),
    Awaygoal = as.numeric(gsub("\\d+-", "", FT)),
    Winner = case_when(
      Homegoal > Awaygoal  ~ "Home",
      Homegoal < Awaygoal  ~ "Away",
      Homegoal == Awaygoal ~ "Draw",
      TRUE ~ "N/A"
    )
  )
