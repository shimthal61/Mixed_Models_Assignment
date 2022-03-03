head(q1_data_tidied)

q1_data_tidied  %>% 
  ggplot(aes(x = Response_Time)) +
  geom_histogram(aes(y =..density..),
                 binwidth = 100,
                 colour = "black", fill = "black",
                 alpha = 0.8) +
  geom_density(alpha = .5, fill = "#FF6666") +
  theme_minimal() +
  labs(x = "Response Time (ms. )",
       y = "Density",
       title = "Density Plot Visualising out Beta Distribution") +
  theme(plot.title = element_text(size = 40, hjust = 0.5, margin = margin(b = 25), line = 0.5, face = "bold"),
        axis.title.x = element_text(size = 30, margin = margin(t = 20)),
        axis.title.y = element_text(size = 30, margin = margin(r = 20)),
        text = element_text(family = "lato", size = 25))
