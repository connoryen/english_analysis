source("Process Completions.R")

df <- degrees_awarded(deg_type = "doctorate", full.data = TRUE)

df %>%
  ggplot(aes(x = year, y = n.students)) + 
  geom_point() +
  geom_line() + 
  theme_minimal()
