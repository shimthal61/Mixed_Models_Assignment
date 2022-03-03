q2_data_tidied %>% 
  ggplot(aes(x = StoryEmotion:FaceExpression, y = RT, colour = FaceExpression)) +
  geom_violin() +
  geom_point(alpha = 0.2, position = position_jitter(width = 0.1, seed = 42)) +
  geom_text()
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
  theme(plot.title = element_text(size = 38, hjust = 0.5, margin = margin(b = 25), line = 0.5, face = "bold"),
        axis.title.x = element_text(size = 38, margin = margin(t = 20)),
        axis.title.y = element_text(size = 38, margin = margin(r = 20)),
        text = element_text(family = "lato", size = 28)) +
  coord_flip()

labels <- q2_descriptives %>% 
  filter(StoryEmotion == "Fear") %>% 
  mutate(label = case_when(FaceExpression == "Anger" ~ "Angry Face\nExpression",
                           FaceExpression == "Fear" ~ "Fearful Face\nExpressions"))
