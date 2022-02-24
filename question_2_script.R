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
