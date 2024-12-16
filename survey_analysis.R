library(tidyverse)

# EDA
survey <- read.csv("survey_results.csv")
survey <- survey %>% select(Q1, Q2, Q3)
survey %>% count(Q1)

survey$yanny_bias <- ifelse(survey$Q1 == "Yanny", 1, 0)
survey$Q2 <- recode(survey$Q2, 'I heard Laurel for all of them'='All Laurel')
survey$Q3 <- recode(survey$Q3, 'I heard Laurel for all of them'='All Laurel', 'I heard Yanny for all of them'='All Yanny')
survey$Q3_recoded <- recode(survey$Q3, '2'='6', '3'='5', '4'='4', '5'='3', '6'='2', '7'='1')

ggplot(survey, aes(x=Q2, fill=Q1)) +
  geom_bar() +
  theme_bw() +
  labs(x="Audio number when perception changed from Laurel to Yanny", fill="Bias")

ggplot(survey, aes(x=Q3, fill=Q1)) +
  geom_bar() +
  theme_bw() +
  labs(x="Audio number when perception changed from Yanny to Laurel", fill="Bias")

survey$laurel_start <- ifelse(survey$Q2 < 4)
survey$yanny_start <- survey$Q3 > 4

survey_reformat <- survey %>% 
  pivot_longer(c(laurel_start, yanny_start), 
                        names_to = "listening_direction", 
                        values_to = "yannyresp") %>% 
  select(c(yanny_bias, listening_direction, yannyresp))

logreg <- glm(yannyresp~yanny_bias+listening_direction+yanny_bias*listening_direction, data=survey_reformat, family=binomial)
summary(logreg)  
