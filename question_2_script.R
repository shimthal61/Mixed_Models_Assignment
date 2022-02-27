q2_data_raw <- read_csv("assignment1_data2.csv")

head(q2_data_raw)

q2_data_tidied <- q2_data_raw %>% 
  mutate(Subject = factor(Subject),
            Vignette = factor(Vignette),
            StoryEmotion = factor(StoryEmotion),
            FaceExpression = factor(FaceExpression))

head(q2_data_tidied)

q2_data_tidied %>% 
  group_by(StoryEmotion, FaceExpression) %>% 
  summarise(mean = mean())

q2_data_tidied %>% 
  ggplot(aes(x = StoryEmotion:FaceExpression, y = RT, colour = FaceExpression)) +
  geom_violin(width = 0.6) +
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
  theme(plot.title = element_text(size = 25, hjust = 0.5, margin = margin(b = 20), line = 0.5, face = "bold"),
        axis.title.x = element_text(size = 20, margin = margin(t = 30)),
        axis.title.y = element_text(size = 20, margin = margin(r = 30)),
        text = element_text(family = "lato", size = 15),
        plot.margin = unit(rep(1.2, 4), "cm")) +
  coord_flip()
