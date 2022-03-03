library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(visdat)
library(fitdistrplus)
library(emmeans)
library(ggthemes)
library(showtext)
library(buildmer)

font_add(family = "lato", regular = "Lato-Regular.ttf")
showtext_auto()

q1_data_raw <- read_csv("assignment1_data1.csv")

head(q1_data_raw)
str(q1_data_raw)

q1_data_tidied <- q1_data_raw %>% 
  transmute(Subject = factor(subj),
            Item = factor(item),
            Condition = factor(condition),
            Response_Time = DV)
head(q1_data_tidied)

str(q1_data_tidied)

head(q1_data_tidied)

(q1_descriptives <- q1_data %>% 
  group_by(Condition) %>% 
  summarise(mean = mean(RT), sd = sd(RT))
)

(q1_plot <- q1_data %>% 
    ggplot(aes(x = Condition, y = RT, colour = Condition)) +
    geom_violin(width = 0.5) +
    geom_point(alpha = 0.2, position = position_jitter(width = 0.08, seed = 42)) +
    guides(colour = 'none') +
    stat_summary(fun.data = 'mean_cl_boot', colour = 'black') +
    labs(x = "Condition",
         y = "Reaction Time (ms)",
         title = "Effect of Item Context on Reaction Time") +
    scale_y_continuous(breaks = seq(0, 6000, by = 1000),
                       limits = c(0, 6000)) +
    theme_minimal() +
    theme(
      text = element_text(family = "lato", size = 25),
      plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = 30, margin = margin(t = 50)),
      axis.title.y = element_text(size = 30, margin = margin(r = 50))) +
    coord_flip())

q1_descriptives %>% 
  ggplot(aes(x = Condition, y = mean)) +
  geom_point()

# If we attempt to build a model which takes into account the random effect of condition, item, and subject, we get a
# warning suggesting we have too many parameters than our data supports

q1_model <- lmer(RT ~ Condition + (1 | Subject) + (1 | Item), data = q1_data)

check_model(q1_model)

x <- check_normality(q1_model)

summary(q1_model)

check_outliers(q1_model)

descdist(q1_data_tidied$Response_Time)

q1_model <- buildmer(Response_Time ~ Condition + (1 + Condition | Subject) + (1 + Condition | Item),
                     data = q1_data_tidied,
                     family = Gamma,
                     nAGQ = 0,
                     buildmerControl = buildmerControl(direction = 'order'))
  formula(q1_model)

view(q1_data_tidied)
  
q1_data_tidied %>% 
  filter(Response_Time > 4000)
  