library(dplyr)
library(ggplot2)
library(tidyr)

grades=read.csv("Course Grades.csv")
pregrades=read.csv("ma20618-1 Sectioning.csv")
#attach(grades)
grades$Instructor=as.factor(grades$Instructor)
grades$Time=as.factor(grades$Time)
grades$Ability=as.factor(grades$Ability)
grades$WPR1Perc=grades$WPR1/125
grades$WPR2Perc=grades$WPR2/125
grades$WPR3Perc=grades$WPR3/125
grades$TEEPerc=grades$TEE/300
grades$WPRAvg=(grades$WPR1Perc+grades$WPR2Perc+grades$WPR3Perc)/3
grades$ExamAvg=(grades$WPR1Perc+grades$WPR2Perc+grades$WPR3Perc+grades$TEEPerc)/4

# Add cqpa column
onlycqpa=pregrades %>% select(First.Name,Last.Name,cqpa)
grades=left_join(grades,onlycqpa,by= c("First.Name"="First.Name","Last.Name"="Last.Name"))

randomsections=grades %>%
  filter(Ability=="0")

grades %>% filter(Ability=="0" & !is.na(cqpa)) %>% dplyr::pull(cqpa) %>% mean
grades %>% filter(Ability=="1" & !is.na(cqpa)) %>% dplyr::pull(cqpa) %>% mean

#lmer - test for random effects

#See if TEE grades are impacted by 
TEEmod= lm(TEEPerc~Time+Instructor+Ability, data=grades)
summary(TEEmod)

#See what impacts WPR1 grades
WPR1mod= lm(WPR1Perc~Time+Instructor+Ability, data=grades)
summary(WPR1mod)

#See what impacts WPR2 grades
WPR2mod= lm(WPR2Perc~Time+Instructor+Ability, data=grades)
summary(WPR2mod)

#See what impacts WPR3 grades
WPR3mod= lm(WPR3Perc~Time+Instructor+Ability, data=grades)
summary(WPR3mod)

#See what impacts WPR Avg
WPRAvgmod= lm(WPRAvg~Time+Instructor+Ability, data=grades)
summary(WPRAvgmod)

#See what impacts Exam Avg
ExamAvgmod= lm(ExamAvg~Time+Instructor+Ability, data=grades)
summary(ExamAvgmod)

#See what impacts Exam Avg for random sections
RandExamAvgmod= lm(ExamAvg~Time+Instructor, data=randomsections)
summary(RandExamAvgmod)


