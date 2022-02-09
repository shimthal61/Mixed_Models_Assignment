library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)

q1_data_raw <- read_csv("assignment1_data1.csv")

head(q1_data_raw)

q1_data <- q1_data_raw %>% 
  mutate(subj = factor(subj),
         item = factor(item),
         condition = factor(condition)) %>% 
  rename(RT = DV)

head(q1_data)

(q1_descriptives <- q1_data %>% 
  group_by(condition) %>% 
  summarise(mean = mean(RT), sd = sd(RT))
)

(q1_plot <- q1_data %>% 
    ggplot(aes(x = condition, y = RT, colour = condition)) +
    geom_violin(width = 0.5) +
    geom_point(alpha = 0.2, position = position_jitter(width = 0.1, seed = 42)) +
    guides(colour = 'none') +
    theme_minimal() +
    stat_summary(fun.data = 'mean_cl_boot', colour = 'black') +
    labs(x = "Condition",
         y = "Reaction Time (ms)",
         title = "Effect of Item Context on Reaction Time") +
    coord_flip()
)

# If we attempt to build a model which takes into account the random effect of condition, item, and subject, we get a
# warning suggesting we have too many parameters than our data supports
q1_model <- lmer(RT ~ condition + (1 + condition | subj) + (1 + condition | item), data = q1_data)

q1_model <- lmer(RT ~ condition + (1 | subj) + (1 | item), data = q1_data)

check_model(q1_model)

summary(q1_model)
