library(EMT)
library(tidyverse)
library(HH)

CadetSect=read.csv("Cadet Sectioned Survey.csv")
CadetNon=read.csv("Cadet Non Sectioned Survey.csv")
CadetLikert=read.csv("Survey Likert Structured.csv")

CadetNonProb=matrix()

#for (i in 1:length(CadetNon$X)){
#  for (j in 2:6){
CadetNonProb=CadetNon[,2:6]/221
CadetSectProb=CadetSect[,2:6]/190
#  }
#}

#CadetSect=as.matrix(CadetSect)
#CadetNonProb=as.matrix(CadetNonProb)
    
###Test Question 5 - This course improved critical thinking####
multinomial.test(as.numeric(CadetSect[1,2:6]),as.numeric(CadetNonProb[1,]),useChisq=TRUE,MonteCarlo=TRUE,ntrial=10000000)
Q5test$p.value#p-value of .1229

###Test Question 6 - WebAssign was a helpful tool####
Q6test=multinomial.test(as.numeric(CadetSect[2,2:6]),as.numeric(CadetNonProb[2,]),useChisq=TRUE,MonteCarlo = TRUE,ntrial=10000000)
Q6test$p.value#p-value of .0209

###Test Question 7 - The PSLs were value added####
Q7test=multinomial.test(as.numeric(CadetSect[3,2:6]),as.numeric(CadetNonProb[3,]),useChisq=TRUE,MonteCarlo = TRUE,ntrial=10000000)
Q7test$p.value#p-value of 0

###Test Question 8 - R improved Tech Comfort####
Q8test=multinomial.test(as.numeric(CadetSect[4,2:6]),as.numeric(CadetNonProb[4,]),useChisq=TRUE,MonteCarlo = TRUE,ntrial=10000000)
Q8test$p.value#p-value of .0151

###Test Question 9 - Helped Analyze Complicated Problems####
Q9test=multinomial.test(as.numeric(CadetSect[5,2:6]),as.numeric(CadetNonProb[5,]),useChisq=TRUE,MonteCarlo = TRUE,ntrial=10000000)
Q9test$p.value#p-value of .0858

###Test Question 10 - More engaged and comfortable participating####useChisq=TRUE,
Q10test=multinomial.test(as.numeric(CadetSect[6,2:6]),as.numeric(CadetNonProb[6,]),useChisq=TRUE,MonteCarlo = TRUE,ntrial=10000000)
Q10test$p.value#p-value of .3537

###Test Question 11 - The pace was too slow to too fast####
Q11test=multinomial.test(as.numeric(CadetSect[7,2:6]),as.numeric(CadetNonProb[7,]),useChisq=TRUE,MonteCarlo = TRUE,ntrial=10000000)
Q11test$p.value#p-value of .5103

###Test Question 12 - My Mathematical Abilities were aligned with the rest of my section####
Q12test=multinomial.test(as.numeric(CadetSect[8,2:6]),as.numeric(CadetNonProb[8,]),useChisq=TRUE,MonteCarlo = TRUE,ntrial=10000000)
Q12test$p.value#p-value of .0189

###Test Question 13 - Sectioning by Ability Advantageous to Disadvantageous####
Q13test=multinomial.test(as.numeric(CadetSect[9,2:6]),as.numeric(CadetNonProb[9,]),useChisq=TRUE,MonteCarlo = TRUE,ntrial=10000000)
Q13test$p.value#p-value of .9022

###Visualize the percentage comparisons
CritThinking=CadetSectProb %>%
  filter(CadetSectProb[1,]) %>%
  ggplot2(aes(y=CadetSectProb[1,])) + 
  geom_point()

Q5= CadetNon[1,2:6]
Q5[2,]=CadetSect[1,2:6]
Q5$Strongly.Agree=Q5$X.5..Strongly.Agree
Q5$Agree=Q5$X.4..Agree
Q5$Neutral=Q5$X.3..Neutral
Q5$Disagree=Q5$X.2..Disagree
Q5$Strongly.Disagree=Q5$X.1..Strongly.Disagree
Q5=Q5[,6:10]
likert(Q5,as.percent=TRUE,main="This Course Improved Your Critical Thinking",sub="p-value = .1696", ylab=c("Non Ability","Ability"))

#Q5 = data.frame(lapply(Q5, factor, ordered=TRUE, levels=1:5, labels=c("Strongly disagree","Disagree","Neutral","Agree","Strongly Agree")))



Q8= CadetNon[4,2:6]
Q8[2,]=CadetSect[4,2:6]
likert(Q8,as.percent=TRUE,main="The Use of R Made Me More Comfortable With Technology",sub="Bottom Comment", ylab=c("Non Ability","Ability"))

##Question 9 likert graph
Q9= CadetNon[5,2:6]
Q9[2,]=CadetSect[5,2:6]
Q9$Strongly.Agree=Q9$X.5..Strongly.Agree
Q9$Agree=Q9$X.4..Agree
Q9$Neutral=Q9$X.3..Neutral
Q9$Disagree=Q9$X.2..Disagree
Q9$Strongly.Disagree=Q9$X.1..Strongly.Disagree
Q9=Q9[,6:10]
likert(Q9,as.percent=TRUE,main="This Course Helped Me Understand and Analyze Complicated Problems",sub="p-value = 0.0858", ylab=c("Ability","Random"))

Q10= CadetNon[6,2:6]
Q10[2,]=CadetSect[6,2:6]
Q10$Strongly.Agree=Q10$X.5..Strongly.Agree
Q10$Agree=Q10$X.4..Agree
Q10$Neutral=Q10$X.3..Neutral
Q10$Disagree=Q10$X.2..Disagree
Q10$Strongly.Disagree=Q10$X.1..Strongly.Disagree
Q10=Q10[,6:10]
likert(Q10,as.percent=TRUE,main="I was Engaged and Felt Comfortable Participating During Class.",sub="p-value = 0.3537", ylab=c("Ability","Random"))

Q12= CadetNon[8,2:6]
Q12[2,]=CadetSect[8,2:6]
Q12$Strongly.Agree=Q12$X.5..Strongly.Agree
Q12$Agree=Q12$X.4..Agree
Q12$Neutral=Q12$X.3..Neutral
Q12$Disagree=Q12$X.2..Disagree
Q12$Strongly.Disagree=Q12$X.1..Strongly.Disagree
Q12=Q12[,6:10]
      likert(Q12,as.percent=TRUE,main="My Mathematical Abilities were Aligned with the Rest of My Section",sub="p-value = 0.0189", ylab=c("Ability","Random"))

Q11= CadetNon[7,2:6]
Q11[2,]=CadetSect[7,2:6]
Q11$Much.To.Slow=Q11$X.5..Strongly.Agree
Q11$A.Little.Slow=Q11$X.4..Agree
Q11$Just.Right=Q11$X.3..Neutral
Q11$A.Little.Fast=Q11$X.2..Disagree
Q11$Much.Too.Fast=Q11$X.1..Strongly.Disagree
Q11=Q11[,6:10]
likert(Q11,as.percent=TRUE,main="The Pace of the Lesson Throughout the Semeseter were:",sub="p-value = 0.5103", ylab=c("Ability","Random"))

Q13= CadetNon[9,2:6]
Q13[2,]=CadetSect[9,2:6]
Q13$Mostly.Advantageous=Q13$X.5..Strongly.Agree
Q13$Somewhat.Advantageous=Q13$X.4..Agree
Q13$Neutral=Q13$X.3..Neutral
Q13$Somewhat.Disadvantageous=Q13$X.2..Disagree
Q13$Mostly.Disadvantageous=Q13$X.1..Strongly.Disagree
Q13=Q13[,6:10]
likert(Q13,as.percent=TRUE,main="Would Sectioning by Ability be Advantageous or Disadvantageous to Learning",sub="p-value = 0.9022", ylab=c("Ability","Random"))


Q10= CadetNon[6,2:6]
Q10[2,]=CadetSect[6,2:6]
likert(Q10,as.percent=TRUE,main="More Comfortable Participating",sub="Bottom Comment", ylab=c("Non Ability","Ability"))

Q12= CadetNon[8,2:6]
Q12[2,]=CadetSect[8,2:6]
likert(Q12,as.percent=TRUE,main="Abilities Aligned with Section",sub="Bottom Comment", ylab=c("Non Ability","Ability"))


CadetSurvey %>% filter(slope=="") %>% 
  ggplot(aes(x=sampling_point, y=sppInv, fill=veg_Type))+
  geom_bar(stat="identity",position="identity")+
  xlab("sampling point")+ylab("number of species")+
  scale_fill_manual(name="Plant type",values = c("#FFA373","#50486D"))+
  coord_flip()+ggtitle("North slope")+
  geom_hline(yintercept=0)+
  xlab("Sampling Points")+
  ylab("Species number")+
  scale_y_continuous(breaks = pretty(vegSurvey$sppInv),labels = abs(pretty(vegSurvey$sppInv)))+
  theme_scientific()
