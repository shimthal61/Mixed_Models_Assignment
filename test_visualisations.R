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
