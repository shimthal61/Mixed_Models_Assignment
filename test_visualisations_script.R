q2_data_tidied  %>% 
  ggplot(aes(x = RT)) +
  geom_histogram(aes(y =..density..),
                 binwidth = 100,
                 colour = "black", fill = "#FF6666",
                 alpha = 0.5) +
  geom_density(alpha = 0, colour = 'blue', lwd = 2) +
  theme_minimal() +
  labs(x = "Reaction Time (ms. )",
       y = "Density",
       title = "Density Plot Visualising out Beta Distribution") +
  theme(plot.title = element_text(size = 40, hjust = 0.5, margin = margin(b = 25), line = 0.5, face = "bold"),
        axis.title.x = element_text(size = 30, margin = margin(t = 20)),
        axis.title.y = element_text(size = 30, margin = margin(r = 20)),
        text = element_text(family = "lato", size = 25))

q1_data_tidied %>% 
  ggplot(aes(x = Context, y = Response_Time, colour = Context)) +
  geom_violin(width = 0.5, lwd = 1.5) +
  geom_point(size = 3, alpha = 0.3, position = position_jitter(width = 0.12, seed = 42)) +
  guides(colour = 'none') +
  stat_summary(fun.data = 'mean_cl_boot', colour = 'black', size = 1) +
  labs(y = "Reaction Time (ms)",
       title = "Effect of Item Context on Reaction Time") +
  scale_y_continuous(breaks = seq(0, 6000, by = 1000),
                     limits = c(0, 6000)) +
  theme_minimal() +
  theme(
    text = element_text(family = "lato", size = 25)) +
  coord_flip()



q2_descriptives <- q2_data_tidied %>% 
  group_by(StoryEmotion, FaceExpression) %>% 
  summarise(mean = mean(RT), sd = sd(RT)) %>% 
  arrange(mean)
head(q2_descriptives)

q2_descriptives %>% 
  ggplot(aes(x = StoryEmotion, y = mean)) +
  geom_line(aes(linetype = FaceExpression, group = FaceExpression, colour = FaceExpression), 
            alpha =0.8, size = 1.2, show.legend = FALSE) +
  geom_point(size = 2.6, aes(colour = FaceExpression), shape = 15) +
  geom_text(size = 8, aes(label = label,
                          colour = FaceExpression),
            data = labels,
            nudge_x = 0.17,
            nudge_y = 70,
            lineheight = 1) +
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
        text = element_text(family = "lato", size = 25))
