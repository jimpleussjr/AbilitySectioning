library(tidyverse)
library(broom)
library(caret)

##read data
finalgrades = read.csv("Course Grades.csv")
tabletojoin = read.csv("joinfromthistable.csv")
tabletojoin = tabletojoin %>% select(NameMelt, New.Final)

names(finalgrades)

## get data in the right shape
finalgrades = finalgrades %>%
  filter(MaxPts.==1000) %>%
  select(-NameMelt, -predictedGrade) %>%
  mutate(test = sub("\\s+[^ ]+$", "", as.character(First.Name))) %>%
  mutate(test2 = paste(Last.Name,test)) %>%
  mutate(NameMelt = gsub(' ', '', as.character(test2))) %>%
  select(-test, -test2) %>%
  left_join(tabletojoin, by = c("NameMelt")) %>%
  filter(!is.na(New.Final)) %>%
  rename(predictedGrade = New.Final)
  

dim(finalgrades)

##create course average, ensure data is being read in the correct format
finalgrades = finalgrades %>%
  mutate(CourseAverage=(WPR1+WPR2+WPR3+TEE)/(300+125+125+125),
         Hour=as.factor(Hour),
         Instructor=as.factor(Instructor),
         Ability=as.factor(Ability)) %>%
  as.tibble()

## determine what section cadets would have been in if they were placed by section for H hour
notabilityH = finalgrades %>%
  filter(Ability==0) %>%
  filter(Hour=="H") %>%
  group_by(Hour)%>%
  arrange(desc(Hour), desc(predictedGrade)) %>%
  select(NameMelt, predictedGrade, Hour, Instructor, Section) 

IfSectionedH = c(rep(1,17),rep(2,17),rep(3,16),rep(4,16),rep(5,16),rep(6,16),rep(7,16),rep(8,16),rep(9,16))

notabilityH$IfSectioned = IfSectionedH

## determine what section cadets would have been in if they were placed by section for C hour
notabilityC = finalgrades %>%
  filter(Ability==0) %>%
  filter(Hour=="C")%>%
  group_by(Hour)%>%
  arrange(desc(Hour), desc(predictedGrade)) %>%
  select(NameMelt, predictedGrade, Hour, Instructor, Section) 

IfSectionedC = c(rep(1,14),rep(2,14),rep(3,14),rep(4,14),rep(5,14),rep(6,14),rep(7,14),rep(8,14),rep(9,14))

notabilityC$IfSectioned = IfSectionedC

## join all data together
fulldata = notabilityC %>%
  bind_rows(notabilityH) %>%
  full_join(finalgrades) %>%
  mutate(IfAllWereSectioned = ifelse(Ability==0,IfSectioned,AbilityLevel)) %>%
  select(NameMelt, predictedGrade, Hour, Instructor, Section, AbilityLevel, Ability, IfSectioned, CourseAverage, IfAllWereSectioned) 

names(fulldata)
#### Verification output and graphs

## verify proper cadets per section
cadetspersection = fulldata %>%
  group_by(Hour, Instructor) %>%
  summarise(n()) %>%
  as.data.frame()

## Create means and sds across each ability group and if they were sectioned
# write.csv(fulldatasums,"fulldatasums.csv", row.names = FALSE)
fulldatasums = fulldata %>% 
  group_by(IfAllWereSectioned, Ability) %>% 
  # select(-predictedGrade)
  summarise(sectionedmean = median(CourseAverage, trim = .05), sectionedsd = sd(CourseAverage), changemean=median(Change, trim=.05), changesd=sd(Change), predmean=median(predictedGrade,trim=.05),predsd=sd(predictedGrade))

fulldata %>% filter(IfAllWereSectioned==1, Ability==1) %>%
  arrange(CourseAverage) %>% select(-Instructor, -Section) %>% print(n=40)


# This shows performace as if they were sectioned by ability
fulldata %>% 
  ggplot(aes(x=IfAllWereSectioned, y = CourseAverage)) +
  geom_point() +
  geom_point(data = fulldatasums, aes(x=IfAllWereSectioned,y=sectionedmean, color = as.factor(IfAllWereSectioned)), inherit.aes = FALSE) +
  # facet_wrap(~Ability) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9),labels = c("Highest", "2","3","4","5","6","7","8","Lowest")) +
  labs(x="Ability Level", y= "Performance", color = NULL)

fulldatasums2=fulldatasums %>% mutate(Ability=as.factor(c(2,3)))

# Jim's attempt to add in the predicted along with the results
# this shows that our results are a bunch of garbage
# write.csv(fulldata, "nottherightwaytodoit2.csv")
fulldata %>%
  ggplot(aes(x=IfAllWereSectioned,y = CourseAverage, color = Ability)) +
  geom_point(position = position_jitterdodge(), alpha = .2) +
  geom_point(data = fulldatasums, aes(x=IfAllWereSectioned,y=sectionedmean, color = Ability), position = position_dodge(width = .5), size = 3.4, inherit.aes = FALSE) +
  geom_errorbar(data = fulldatasums, aes(x=IfAllWereSectioned,y = NULL, ymax=sectionedmean+sectionedsd, ymin=sectionedmean-sectionedsd, 
                colour=Ability), width=.4, size = 1.2, position = position_dodge(width = 0.5), inherit.aes = FALSE) +
  geom_errorbar(data = fulldatasums2, aes(x=IfAllWereSectioned,y = NULL, ymax=predmean+predsd, ymin=predmean-predsd,
                colour=Ability), width=.2, size = 1, position = position_dodge(width = 1.4), inherit.aes = FALSE) +
  scale_colour_discrete(name="",
                        h=c(100,105),
                        l=c(30,60,45,70),
                breaks=c(2,0, 1,3),
                labels=c("Random Predicted","Random Actual", "Ability Actual", "Ability Predicted")) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9),labels = c("Highest", "2","3","4","5","6","7","8","Lowest")) +
  #labs(title = "MA206: Probability and Statistics Performance By Ability Level", x = "Ability", y = "Performance") +
  labs( x = "Ability", y = "Performance") +
  theme(legend.position="bottom")+
  ylim(0.5,1.0)


# this shows that our results are a bunch of garbage
# write.csv(fulldata, "nottherightwaytodoit2.csv")
fulldata %>%
  ggplot(aes(x=IfAllWereSectioned,y = CourseAverage, color = Ability)) +
  geom_point(position = position_jitterdodge(), alpha = .2) +
  #geom_errorbar(aes(x=IfAllWereSectioned,y = CourseAverage,ymax=mean(predictedGrade)+sd(predictedGrade), ymin=mean(predictedGrade)-sd(predictedGrade), 
  #                                       colour="blue"), width=.5, size = 1.5, position = position_dodge(width = 0.7), inherit.aes = FALSE) +
  #geom_point(position = position_jitterdodge(), alpha = .2, aes(color="blue")) + 
  geom_point(data = fulldatasums, aes(x=IfAllWereSectioned,y=sectionedmean, color = Ability), position = position_dodge(width = .7), size = 5, inherit.aes = FALSE) +
  #geom_point(data=fulldatasums,aes(x=IfAllWereSectioned,y=predmean,color="blue"), position = position_dodge(width = .7), size = 5, inherit.aes = FALSE) +
  # geom_errorbar(data = fulldatasums, aes(ymin=sectionedmean-sectionedsd, ymax=sectionedmean+sectionedsd, 
  #                 colour=Ability), width=.5, size = 1.5, position = position_dodge(width = 0.4)) +
  geom_errorbar(data = fulldatasums, aes(x=IfAllWereSectioned,y = NULL, ymax=sectionedmean+sectionedsd, ymin=sectionedmean-sectionedsd, 
                                         colour=Ability), width=.5, size = 1.5, position = position_dodge(width = 0.7), inherit.aes = FALSE) +
  scale_colour_discrete(name  ="Class Type",
                        breaks=c(0, 1),
                        labels=c("Randomly Assigned", "Ability Grouped")) +
  #geom_errorbar(data = fulldatasums,aes(x=IfAllWereSectioned,y=predmean,ymax=predmean+predsd,ymin=predmean-predsd, 
  #  colour="blue"),width=.3,size=1.5,position=position_jitterdodge(),inherit.aes=FALSE) +
  #scale_colour_discrete(name  ="Prediction",
  #                      breaks=c(0, 1),
  #                      labels=c("Randomly Assigned", "Ability Grouped")) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9),labels = c("Highest", "2","3","4","5","6","7","8","Lowest")) +
  #labs(title = "MA206: Probability and Statistics Performance By Ability Level", x = "Ability", y = "Performance") +
  labs( x = "Ability", y = "Performance") +
  theme(legend.position="bottom")+
  ylim(0.5,1.0)


#### Same graph but with predicted Grade

fulldata %>%
  ggplot(aes(x=IfAllWereSectioned,y = predictedGrade, color = Ability)) +
  geom_point(position = position_jitterdodge(), alpha = .2) +
  #geom_errorbar(aes(x=IfAllWereSectioned,y = CourseAverage,ymax=mean(predictedGrade)+sd(predictedGrade), ymin=mean(predictedGrade)-sd(predictedGrade), 
  #                                       colour="blue"), width=.5, size = 1.5, position = position_dodge(width = 0.7), inherit.aes = FALSE) +
  #geom_point(position = position_jitterdodge(), alpha = .2, aes(color="blue")) + 
  geom_point(data = fulldatasums, aes(x=IfAllWereSectioned,y=predmean, color = Ability), position = position_dodge(width = .7), size = 3.5, inherit.aes = FALSE) +
  #geom_point(data=fulldatasums,aes(x=IfAllWereSectioned,y=predmean,color="blue"), position = position_dodge(width = .7), size = 5, inherit.aes = FALSE) +
  # geom_errorbar(data = fulldatasums, aes(ymin=sectionedmean-sectionedsd, ymax=sectionedmean+sectionedsd, 
  #                 colour=Ability), width=.5, size = 1.5, position = position_dodge(width = 0.4)) +
  geom_errorbar(data = fulldatasums, aes(x=IfAllWereSectioned,y = NULL, ymax=predmean+predsd, ymin=predmean-predsd, 
                                         colour=Ability), width=.5, size = 1.3, position = position_dodge(width = 0.7), inherit.aes = FALSE) +
  #scale_colour_hue(h=c(30,250)) +
  scale_colour_manual(name  ="Class Type",
                        breaks=c(0, 1),
                        labels=c("Randomly Assigned", "Ability Grouped"),
                        values=c("blue","orange")) +
   
  #geom_errorbar(data = fulldatasums,aes(x=IfAllWereSectioned,y=predmean,ymax=predmean+predsd,ymin=predmean-predsd, 
  #  colour="blue"),width=.3,size=1.5,position=position_jitterdodge(),inherit.aes=FALSE) +
  #scale_colour_discrete(name  ="Prediction",
  #                      breaks=c(0, 1),
  #                      labels=c("Randomly Assigned", "Ability Grouped")) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9),labels = c("Highest", "2","3","4","5","6","7","8","Lowest")) +
  labs(title = "", x = "Ability", y = "Predicted Performance") +
  theme(legend.position="bottom") +
  ylim(0.5,1.0)

# this shows that our results are a bunch of garbage
# write.csv(fulldata, "nottherightwaytodoit2.csv")
fulldata %>%
  ggplot(aes(x=IfAllWereSectioned,y = Change, color = Ability)) +
  geom_point(position = position_jitterdodge(), alpha = .2) +
  geom_point(data = fulldatasums, aes(x=IfAllWereSectioned,y=changemean, color = Ability), position = position_dodge(width = .7), size = 5, inherit.aes = FALSE) +
  # geom_errorbar(data = fulldatasums, aes(ymin=sectionedmean-sectionedsd, ymax=sectionedmean+sectionedsd, 
  #                 colour=Ability), width=.5, size = 1.5, position = position_dodge(width = 0.4)) +
  geom_errorbar(data = fulldatasums, aes(x=IfAllWereSectioned,y = NULL, ymax=changemean+changesd, ymin=changemean-changesd, 
                                         colour=Ability), width=.5, size = 1.5, position = position_dodge(width = 0.7), inherit.aes = FALSE) +
  scale_colour_discrete(name  ="Ability",
                        breaks=c(0, 1),
                        labels=c("Randomly Assigned", "Ability Grouped")) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9),labels = c("Highest", "2","3","4","5","6","7","8","Lowest")) +
  labs(title = "MA206: Probability and Statistics Performance By Ability Level", x = "Ability", y = "Change") +
  theme(legend.position="bottom")

# fulldata %>%
#   ggplot(aes(x=IfAllWereSectioned,y = CourseAverage, color = Ability)) +
#   geom_point(position = position_jitterdodge(), alpha = .2) +
#   geom_point(data = fulldatasums, aes(y=sectionedmean), position = position_dodge(width = .7), size = 5) +
#   # geom_errorbar(data = fulldatasums, aes(ymin=sectionedmean-sectionedsd, ymax=sectionedmean+sectionedsd,
#   #                 colour=Ability), width=.5, size = 1.5, position = position_dodge(width = 0.4)) +
#   geom_errorbar(data = fulldatasums, aes(y = NULL, ymax=sectionedmean+sectionedsd, ymin=sectionedmean-sectionedsd,
#     colour=Ability), width=.5, size = 1.5, position = position_dodge(width = 0.7)) +
#   scale_colour_discrete(name  ="Ability",
#                         breaks=c(0, 1),
#                         labels=c("Randomly Assigned", "Ability Grouped")) +
#   labs(title = "MA206: Probability and Statistics Performace By Ability", x = "Ability", y = "Performance")
# 
  

## t test comparisons

# t.test of those who were in ability sections vs those who weren't -- no difference
myt = t.test(fulldata$CourseAverage[fulldata$Ability==0],fulldata$CourseAverage[fulldata$Ability==1])
sd(fulldata$CourseAverage[fulldata$Ability==0])
sd(fulldata$CourseAverage[fulldata$Ability==1])

myt

# t.test of each ability withing whether or not they were sectioned
library(broom)
options(scipen = 999)
fulldata %>% ungroup() %>%
  select(CourseAverage, Ability, IfAllWereSectioned) %>% 
  group_by(IfAllWereSectioned) %>% 
  do(tidy(t.test(CourseAverage~Ability, data=.)))

## this matches the t.test above so I am confidente my code above works
# create one example
data0 = fulldata %>% filter(Ability==0, IfAllWereSectioned==1)
data1 = fulldata %>% filter(Ability==1, IfAllWereSectioned==1)
# execute one example
t.test(data0$CourseAverage,data1$CourseAverage)




## this is where I did the lm to determine what impacted the actual course average....I'm sorry it looks like garbage
summary(lm(fulldata$CourseAverage~as.factor(fulldata$Ability):as.factor(fulldata$IfAllWereSectioned)))

fulldata$Hour2 = ifelse(fulldata$Hour=="B"|fulldata$Hour=="H","BH","CI")

mod=lm(CourseAverage~Ability*as.factor(IfAllWereSectioned)-1, data = fulldata)
mod=lm(CourseAverage~Ability:as.factor(IfAllWereSectioned)+predictedGrade-1, data = fulldata)
summary(mod)
mod=lm(CourseAverage~predictedGrade+as.factor(IfAllWereSectioned)*Ability-1, data = fulldata)
mod=lm(CourseAverage~predictedGrade+Ability-1, data = fulldata)
summary(mod)

mod$coefficients

mod=lm(CourseAverage~predictedGrade+Ability+Hour2-1, data = fulldata)
summary(mod)


fulldata %>%
  filter(IfAllWereSectioned==1,Ability==0) %>%
  summarise(mean(CourseAverage))

coefplot::coefplot(mod)


## just for fun

summarygrades = 
  fulldata %>%
  group_by(Ability, Section) %>%
  summarise(meangrade = mean(CourseAverage), sdgrade = sd(CourseAverage)) 

summarygrades %>%
  ggplot(aes(y=meangrade,x=Ability)) +
  geom_point() +
  facet_wrap(~Ability, scales = "free_x")

names(fulldata)

####  Add column that looks at hour of each student and one that looks at the difference in predicted verses actual

fulldata = fulldata %>% 
  mutate(Time=ifelse(Hour=="H"|Hour=="B","H2","H3")) %>%
  mutate(Change=CourseAverage-predictedGrade)

### controlling for time period, instructor, and ability

summary(lm(CourseAverage~Ability+Time+as.factor(IfAllWereSectioned)+Instructor,data=fulldata))
summary(lm(CourseAverage~Ability+Time+predictedGrade+Instructor-1,data=fulldata))
summary(lm(Change~Ability:as.factor(IfAllWereSectioned),data=fulldata))

### See how different predicted performance is across ability verses random sections

fulldata %>%
  group_by(as.factor(IfAllWereSectioned),Ability) %>%
  #filter(Ability==1) %>%
  summarise(mean(predictedGrade))



fulldata %>%
  select(Change, Ability) %>%
  group_by(Ability) %>%
  summarise(sd(Change))

t.test(fulldata$predictedGrade[fulldata$Ability==1 & fulldata$IfAllWereSectioned==1],fulldata$predictedGrade[fulldata$Ability==0& fulldata$IfAllWereSectioned==1])
t.test(fulldata$CourseAverage[fulldata$Ability==1 & fulldata$IfAllWereSectioned==9],fulldata$CourseAverage[fulldata$Ability==0& fulldata$IfAllWereSectioned==9])

t.test(fulldata$predictedGrade[fulldata$Ability==1 & fulldata$IfAllWereSectioned==2],fulldata$predictedGrade[fulldata$Ability==0& fulldata$IfAllWereSectioned==2])
t.test(fulldata$predictedGrade[fulldata$Ability==1 & fulldata$IfAllWereSectioned==3],fulldata$predictedGrade[fulldata$Ability==0& fulldata$IfAllWereSectioned==3])


#################
###How well did our model predict
#################
