q2_data_raw <- read_csv("assignment1_data2.csv")

head(q2_data_raw)

q2_data_tidied <- q2_data_raw %>% 
  mutate(Subject = factor(Subject),
            Vignette = factor(Vignette),
            StoryEmotion = factor(StoryEmotion),
            FaceExpression = factor(FaceExpression))

head(q2_data_tidied)

q2_descriptives <- q2_data_tidied %>% 
  group_by(StoryEmotion, FaceExpression) %>% 
  summarise(mean = mean(RT), sd = sd(RT)) %>% 
  arrange(mean)
head(q2_descriptives)

q2_data_tidied %>% 
  ggplot(aes(x = StoryEmotion:FaceExpression, y = RT, colour = FaceExpression)) +
  geom_violin() +
  geom_point(alpha = 0.2, position = position_jitter(width = 0.1, seed = 42)) +
  stat_summary(fun.data = 'mean_cl_boot', colour = 'black') +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 10000, by = 1250),
                     limits = c(0, 10000)) +
  scale_x_discrete(labels = c("Anger:Anger" = "Anger",
                              "Anger:Fear" = "Anger",
                              "Fear:Anger" = "Fear",
                              "Fear:Fear" = "Fear")) +
  labs(x = "Story Emotion",
       y = "Reaction Time (ms. )",
       title = "Examining the Effect of Story Emotion\n and Face Expression on Reaction Time") +
  theme(plot.title = element_text(size = 40, hjust = 0.5, margin = margin(b = 25), line = 0.5, face = "bold"),
        axis.title.x = element_text(size = 30, margin = margin(t = 20)),
        axis.title.y = element_text(size = 30, margin = margin(r = 20)),
        text = element_text(family = "lato", size = 25)) +
  coord_flip()


labels <- q2_descriptives %>% 
  filter(StoryEmotion == "Fear") %>% 
  mutate(label = case_when(FaceExpression == "Anger" ~ "Angry Face\nExpression",
                           FaceExpression == "Fear" ~ "Fearful Face\nExpressions"))
head(labels)

q2_descriptives %>% 
  ggplot(aes(x = StoryEmotion, y = mean)) +
  geom_line(size = 1.2, aes(group = FaceExpression, colour = FaceExpression)) +
  geom_point(size = 2.6, aes(colour = FaceExpression), shape = 15) +
  geom_text(size = 8, aes(label = label,
                          colour = FaceExpression),
            data = labels,
            nudge_x = 0.17,
            nudge_y = 70,
            lineheight = 0.5) +
  guides(colour = 'none') +
  scale_y_continuous(breaks = seq(1800, 2800, by = 200),
                     limits = c(1800, 2800)) +
  labs(x = "Story Emotion",
       y = "Reaction Time (ms. )",
       title = "Examining the Interaction Between\nStory Emotion and Face Expression") +
  theme_hc() +
  theme(plot.title = element_text(size = 40, hjust = 0.5, line = 0.5, margin = margin(b = 25), face = "bold"),
        axis.title.x = element_text(size = 30, margin = margin(t = 20)),
        axis.title.y = element_text(size = 30, margin = margin(r = 20)),
        text = element_text(family = "lato", size = 25)
  )

contrasts(q2_data_tidied$StoryEmotion) <- matrix(c(.5, -.5))
contrasts(q2_data_tidied$FaceExpression) <- matrix(c(.5, -.5))

# Maximal model
q2_model_max <- lmer(RT ~ StoryEmotion * FaceExpression +
                   (1 + StoryEmotion * FaceExpression | Subject) +
                   (1 + StoryEmotion * FaceExpression | Vignette),
                 data = q2_data_tidied)

q2_buildmer_model <- buildmer(RT ~ StoryEmotion * FaceExpression +
                                (1 + StoryEmotion * FaceExpression | Subject) +
                                (1 + StoryEmotion * FaceExpression | Vignette),
                              data = q2_data_tidied)

summary(q2_buildmer_model)       


q2_max_feas_model <- lmer(RT ~ 1 + FaceExpression + StoryEmotion + FaceExpression:StoryEmotion + 
                            (1 + FaceExpression | Subject) + 
                            (1 + FaceExpression | Vignette),
                          data = q2_data_tidied)


check_model(q2_max_feas_model)

summary(q2_max_feas_model)

emmeans(q2_max_feas_model)

head(q2_data_tidied)

# Histogram
q2_data_tidied  %>% 
  ggplot(aes(x = RT)) +
  geom_histogram(aes(y =..density..),
                 binwidth = 100,
                 colour = "black", fill = "black",
                 alpha = 0.8) +
  theme_minimal() +
  labs(x = "Reaction Time (ms. )",
       y = "Density",
       title = "Density Plot Visualising out Beta Distribution") +
  theme(plot.title = element_text(size = 40, hjust = 0.5, margin = margin(b = 25), line = 0.5, face = "bold"),
        axis.title.x = element_text(size = 30, margin = margin(t = 20)),
        axis.title.y = element_text(size = 30, margin = margin(r = 20)),
        text = element_text(family = "lato", size = 25))

# Density Plot
q2_data_tidied  %>% 
  ggplot(aes(x = RT)) +
  geom_density(fill = "#FF6666") +
  theme_minimal() +
  labs(x = "Reaction Time (ms. )",
       y = "Density",
       title = "Density Plot Visualising out Beta Distribution") +
  theme(plot.title = element_text(size = 40, hjust = 0.5, margin = margin(b = 25), line = 0.5, face = "bold"),
        axis.title.x = element_text(size = 30, margin = margin(t = 20)),
        axis.title.y = element_text(size = 30, margin = margin(r = 20)),
        text = element_text(family = "lato", size = 25))

# Histogram and Density Plot
q2_data_tidied  %>% 
  ggplot(aes(x = RT)) +
  geom_histogram(aes(y =..density..),
                 binwidth = 100,
                 colour = "black", fill = "black",
                 alpha = 0.8) +
  geom_density(alpha = .5, fill = "#FF6666") +
  theme_minimal() +
  labs(x = "Reaction Time (ms. )",
       y = "Density",
       title = "Density Plot Visualising out Beta Distribution") +
  theme(plot.title = element_text(size = 40, hjust = 0.5, margin = margin(b = 25), line = 0.5, face = "bold"),
        axis.title.x = element_text(size = 30, margin = margin(t = 20)),
        axis.title.y = element_text(size = 30, margin = margin(r = 20)),
        text = element_text(family = "lato", size = 25))