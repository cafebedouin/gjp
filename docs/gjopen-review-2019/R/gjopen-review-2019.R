setwd("/home/scott/Documents/R/forecasting/docs/gjopen-review-2019")
library("dplyr")
library("tidyr")
library("ggplot2")
library("bbplot")

# CSV in the following format
# Date Ended,Question,Participation Rate,Brier Score,Median Score,Accuracy Score 
# "Dec 15, 2018 12:00PM ","Before 15 December 2018...",85%,0.004,0,0.003
scores <- read.csv("data/gjopen-scores-2019.csv", skip=0, header=TRUE)
scores$Date.Ended <- as.Date(scores$Date.Ended, format = '%b %d, %Y %I:%M%p')

# Uncomment or reorder fields if you want to use a different date
# scores$Date.Ended <- as.Date(scores$Date.Ended, format = '%Y-%m-%d')

colnames(scores) <- c("Date", "Question", "Time", "Brier", "Median", "Accuracy")
scores$Time <- as.numeric(gsub("[\\%,]", "", scores$Time))
scores$Brier <- as.numeric(scores$Brier)
scores$Median <- as.numeric(scores$Median)
scores$Accuracy <- as.numeric(scores$Accuracy)
scores$Question <- substr(scores$Question, 0, 40)
# outliers <- mutate(index = seq(1:length(outliers$gap)))
outliers <- filter(scores, 
                  (Accuracy >= .10) |
                  (Accuracy <= -.10) | 
                  (Brier > .75)) %>%
                  select(Date, Question, Time, Brier, Median, Accuracy) %>%
#                  spread(Median, Brier) %>%
#                  mutate(Gap = Brier - Median) %>%
                  arrange(desc(Accuracy)) 
View(outliers)
View(scores)



#ggplot(scores, aes(Brier)) +
#  geom_histogram(binwidth = 0.25, colour = "white", fill = "#1380A1") +
#  geom_hline(yintercept = 0, size = 1, colour="#333333") +
#  bbc_style() +
#  scale_x_continuous(limits = c(0.0, 2),
#                     breaks = seq(0.0, 2, by = 0.25),
#                     labels = c("0.0", "0.25", "0.5", "0.75", "1.0", 
#                                "1.25", "1.5", "1.75", "2.0")) +
#  labs(title = "Brier Scores, 2019",
#       subtitle = "")


