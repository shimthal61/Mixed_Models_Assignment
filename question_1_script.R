library(tidyverse)

q1_data_raw <- read_csv("assignment1_data1.csv")

head(q1_data_raw)

q1_data <- q1_data_raw %>% 
  mutate(subj = factor(subj),
         item = factor(item),
         condition = factor(condition)) %>% 
  rename(RT = DV)

head(q1_data)

(q1_data_plot <- q1_data %>% 
    ggplot(aes(x = condition, y = RT, colour = condition)) +
    geom_violin(width = 0.5) +
    geom_point(alpha = 0.5, position = position_jitter(width = 0.1, seed = 42)) +
    guides(colour = FALSE) +
    theme_minimal() +
    stat_summary(fun.data = 'mean_cl_boot', colour = 'black') +
    labs(x = "Condition",
         y = "Reaction Time (ms)",
         title = "Effect of Item Context on Reaction Time") +
    coord_flip()
)
