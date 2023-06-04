# Load data

csvpath <- "https://raw.githubusercontent.com/footballcsv/england/master/2010s/2019-20/eng.1.csv"
results_init <- read.csv(csvpath)

# get date in right format
results <- results %>% 
  mutate(
    Day = as.Date(Date, format = "%a %b %d %Y")
  )
