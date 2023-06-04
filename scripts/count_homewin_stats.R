# Count home win stats
#
# Can we check whether the home advantage is merely a myth,
# or is a real thing?
#

data <- results %>% 
  group_by(Winner) %>% 
  summarise(
    n = n(),
    Percentage = percent(n/nrow(.))
  ) %>% 
  ungroup() %>% 
  arrange(-Percentage)

data %>% formattable()
