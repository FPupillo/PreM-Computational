# RL analysis
rm(list=ls())
library(dplyr)
library(ggplot2)
library(lme4)

# data from RL
#RLdata<-read.csv("output_files/Dirichlet.csv")
#RLdata<-read.csv("output_files/RLdata_bayesian.csv")
#RLdata<-read.csv("output_files/RLdata_obsALL.csv")
RLdata<-read.csv("output_files/RLdata_bayesian.csv")

# memory data
recogData<-read.csv("output_files/recognitionData.csv")

# in RL data, cut all the trial before the 16th
RLdata<-RLdata[RLdata$trialNum>15,]

# count
countT<- RLdata %>% 
  group_by(SubNum)%>%
  tally()

count<- recogData %>% 
  group_by(SubNum)%>%
  tally()


# delete all the butterflies from the RL
RLdata<-RLdata[(RLdata$objfile!= "butterfly1.jpg" & RLdata$objfile!= "butterfly2.jpg"&
                     RLdata$objfile!= "butterfly3.jpg" & RLdata$objfile!= "butterfly4.jpg" &
                     RLdata$objfile!= "butterfly5.jpg"), ]

# delete dtafrom participant 5 and 10
#RLdata<-RLdata[!RLdata$SubNum %in% c(5, 10), ]
#recogData<-recogData[!recogData$SubNum %in% c(5, 10), ]


# attach Prediction error to the recog data
# now prediction error
RLdata$Delta<-NA
RLdata$respFlower<-as.numeric(RLdata$respFlower)
for (j in 1:nrow(RLdata)){
  
  if (!is.na(RLdata$FlowNum[j])){
    
    RLdata$Delta[j]<-RLdata[j, paste("Delta", RLdata$FlowNum[j], sep="")]
    
  }
}

recogData$Delta<-RLdata$Delta

# and uncertainty
recogData$uncertainty<-RLdata$uncertainty
# learning rate
recogData$lr<-RLdata$lr

# and CPP
recogData$CPP<-RLdata$CPP

# esclude participant 2

recogData<-recogData[recogData$listN!=2,]

# first, PE. Distribution of PE by before and after CP

# distribution of PE by condition
RLdata$befAft<-as.factor(RLdata$befAft)
levels(RLdata$befAft)<- c("afterCP", "beforeCP")

PErespdistr<-ggplot(RLdata, aes(x= Delta, fill=befAft))
PErespdistr+geom_histogram()+ facet_grid( ~ befAft)

PErespdistr<-ggplot(RLdata, aes(y= Delta, x=befAft, fill=befAft))
PErespdistr+geom_boxplot()


# distribution of lr by condition
PErespdistr<-ggplot(RLdata, aes(x= lr, fill=befAft))
PErespdistr+geom_histogram()+ facet_grid( ~ befAft)

PErespdistr<-ggplot(RLdata, aes(x= Delta, fill=befAft))
PErespdistr+geom_histogram()+ facet_grid( ~ befAft)

PErespdistr<-ggplot(RLdata, aes(y= lr, x=befAft, fill=befAft))

PErespdistr+geom_boxplot()


# distribution of uncertainty
PErespdistr<-ggplot(RLdata, aes(x= uncertainty, fill=befAft))
PErespdistr+geom_histogram()+ facet_grid( ~ befAft)

PErespdistr<-ggplot(RLdata, aes(y= uncertainty, x=befAft, fill=befAft))
PErespdistr+geom_boxplot()


# uncertainty  as the variance of the probabilities
ggplot(RLdata[RLdata$SubNum==2,], aes(x=trialNum))+
  
  geom_line(aes(y=uncertainty), size = 1.5, color = "blue")

# uncertainty  as the variance of the probabilities
ggplot(RLdata, aes(x=trialNum))+
  geom_line(aes(y=uncertainty),stat="summary", fun = "mean", size = 1.5, color = "blue")


# distribution of CPP
PErespdistr<-ggplot(RLdata, aes(y= 1-CPP, x=befAft, fill=befAft))
PErespdistr+geom_boxplot()


# CP
ggplot(RLdata, aes(x=trialNum))+
  
  geom_line(aes(y=CPP), size = 1.5, color = "blue")

# plot PE
ggplot(recogData, aes(Delta, recogAcc))+
  geom_smooth(method="glm",formula=y~poly(x, 2),method.args=list(family="binomial"), se=F)+aes(colour = factor(SubNum))+
  geom_smooth(method="glm",formula=y~poly(x, 2),method.args=list(family="binomial"), colour="black", se=T)+
  # facet_grid(.~accuracy)+
  theme(strip.text.x = element_text(size = 13))

# plot LR
ggplot(recogData, aes(lr, recogAcc))+
  geom_smooth(method="glm",formula=y~poly(x, 2),method.args=list(family="binomial"), se=F)+aes(colour = factor(SubNum))+
  geom_smooth(method="glm",formula=y~poly(x, 2),method.args=list(family="binomial"), colour="black", se=T)+
  # facet_grid(.~accuracy)+
  theme(strip.text.x = element_text(size = 13))

# plot uncertainty
ggplot(recogData, aes(uncertainty, recogAcc))+
  geom_smooth(method="glm",formula=y~poly(x, 2),method.args=list(family="binomial"), se=F)+aes(colour = factor(SubNum))+
  geom_smooth(method="glm",formula=y~poly(x, 2),method.args=list(family="binomial"), colour="black", se=T)+
  # facet_grid(.~accuracy)+
  theme(strip.text.x = element_text(size = 13))

# change point
ggplot(recogData, aes(CPP, recogAcc))+
  geom_smooth(method="glm",formula=y~poly(x, 2),method.args=list(family="binomial"), se=F)+aes(colour = factor(SubNum))+
  geom_smooth(method="glm",formula=y~poly(x, 2),method.args=list(family="binomial"), colour="black", se=T)+
  # facet_grid(.~accuracy)+
  theme(strip.text.x = element_text(size = 13))



modUNCERT<-glmer(recogAcc~poly(uncertainty, 2)+(1+uncertainty|SubNum), family=binomial(),
             data=recogData[recogData$listN!=2,])

summary(modUNCERT)

# create quantiles
UNquant<-quantile(recogData$uncertainty[recogData$listN!=2], probs=c(0.25, 0.50, 0.75), na.rm=T)

recogData$Unquant<-NA
for ( n in 1:nrow(recogData)){
  if (!is.na(recogData$uncertainty[n])){
    if (recogData$uncertainty[n]<UNquant[1]){
      recogData$UNquant[n]<-1
    } else if (recogData$uncertainty[n]>UNquant[1] & recogData$uncertainty[n]<UNquant[2]  ) {
      recogData$UNquant[n]<-2
    } else if (recogData$uncertainty[n]>UNquant[2] & recogData$uncertainty[n]<UNquant[3]  ){
      recogData$UNquant[n]<-3
    } else {  recogData$UNquant[n]<-4}
  }
}

recogData$UNquant<-as.factor(recogData$UNquant)
ggplot(recogData[recogData$listN!=2,], aes(x=UNquant, y= recogAcc))+
  geom_bar(aes(UNquant, recogAcc, fill = UNquant),position="dodge",stat="summary", fun.y="mean", SE=T) +
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  #facet_grid(.~rec_session)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab(" % Hit")+
  # xlab("feedback")+
  #scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14))


# plot uncertainty
ggplot(recogData, aes(x=trialNum))+
  
  geom_line(aes(y=uncertainty), size = 1.5, color = "blue")+
  geom_vline(xintercept = c(52,88, 124))+
  geom_hline(yintercept= 0.91)+
  geom_hline(yintercept = 0.934)


modaccuracy<-glmer(recogAcc~accuracy+(1|SubNum), family=binomial(),
                   data=recogData[recogData$listN!=2,])

summary(modaccuracy)

# mpodle it
modPE<-glmer(recogAcc~PE+(PE|SubNum), family=binomial(),
             data=recogData[recogData$listN!=2,])

summary(modPE)
modPEacc<-glmer(recogAcc~accuracy+PE+(PE+accuracy|SubNum), family=binomial(),
             data=recogData[recogData$listN!=2,])
summary(modPEacc)

# with accuracy
ggplot(recogData, aes(PE, recogAcc))+
  geom_smooth(method="glm",formula=y~x,method.args=list(family="binomial"), se=F)+aes(colour = factor(SubNum))+
  geom_smooth(method="glm",formula=y~x,method.args=list(family="binomial"), colour="black", se=T)+
  facet_grid(.~accuracy)+
  theme(strip.text.x = element_text(size = 13))

# ggplot(recogData, aes(SI, recogAcc))+
#   geom_smooth(method="glm",formula=y~x,method.args=list(family="binomial"), se=F)+aes(colour = factor(SubNum))+
#   geom_smooth(method="glm",formula=y~x,method.args=list(family="binomial"), colour="black", se=T)+
#   # facet_grid(.~accuracy)+
#   theme(strip.text.x = element_text(size = 13))


# ggplot(recogData[recogData$SubNum!=1,], aes(lr, recogAcc))+
#   geom_smooth(method="glm",formula=y~x,method.args=list(family="binomial"), se=F)+aes(colour = factor(SubNum))+
#   geom_smooth(method="glm",formula=y~x,method.args=list(family="binomial"), colour="black", se=T)+
#   # facet_grid(.~accuracy)+
#   theme(strip.text.x = element_text(size = 13))

# remove outlier
# recogDataout<-recogData[recogData$lr<0.015,]
# 
# ggplot(recogDataout, aes(lr, recogAcc))+
#   geom_smooth(method="glm",formula=y~x,method.args=list(family="binomial"), se=F)+aes(colour = factor(SubNum))+
#   geom_smooth(method="glm",formula=y~x,method.args=list(family="binomial"), colour="black", se=T)+
#   # facet_grid(.~accuracy)+
#   theme(strip.text.x = element_text(size = 13))
# 
# hist(recogDataout$lr)
# 
# modlr<-glmer(recogAcc~lr+accuracy+ (lr|SubNum), family=binomial(), data= recogDataout)
# summary(modlr)

# create bins
# get PE quantiles
PEquant<-quantile(recogData$PE, probs=c(0.25, 0.50, 0.75), na.rm=T)

#PEquant<-c(-0.5, 0, 0.5 )
recogData$PEquant<-NA
for ( n in 1:nrow(recogData)){
  if (!is.na(recogData$PE[n])){
  if (recogData$PE[n]<PEquant[1]){
  recogData$PEquant[n]<-1
  } else if (recogData$PE[n]>PEquant[1] & recogData$PE[n]<PEquant[2]  ) {
    recogData$PEquant[n]<-2
  } else if (recogData$PE[n]>PEquant[2] & recogData$PE[n]<PEquant[3]  ){
    recogData$PEquant[n]<-3
  } else {  recogData$PEquant[n]<-4}
  }
}


recogData$PEquant<-as.factor(recogData$PEquant)
ggplot(recogData[complete.cases(recogData),], aes(x=PEquant, y= recogAcc))+
  geom_bar(aes(PEquant, recogAcc, fill = PEquant),position="dodge",stat="summary", fun.y="mean", SE=T) +
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  #facet_grid(.~rec_session)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab(" % Hit")+
  # xlab("feedback")+
  #scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14))

# bly list n
ggplot(recogData[complete.cases(recogData),], aes(x=PEquant, y= recogAcc))+
  geom_bar(aes(PEquant, recogAcc, fill = PEquant),position="dodge",stat="summary", fun.y="mean", SE=T) +
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  facet_grid(.~listN)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab(" % Hit")+
  # xlab("feedback")+
  #scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14))


# median split
med<-median(recogData$uncertainty)
recogData$highvslowUn<-NA
for (n in 1:nrow(recogData) ){
  if (recogData$uncertainty[n]>med){
    recogData$highvslowUn[n]<-"high"
  } else{    recogData$highvslowUn[n]<-"lower"
}
}

ggplot(recogData, aes(x=highvslowUn, y= recogAcc))+
  geom_bar(aes(highvslowUn, recogAcc, fill = highvslowUn),position="dodge",stat="summary", fun.y="mean", SE=T) +
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  theme_bw()+
  facet_grid(.~listN)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab(" % Hit")+
  # xlab("feedback")+
  #scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14))




# model it
modPE<-glmer(recogAcc~PE+accuracy+(1+ PE|SubNum), family=binomial(),
             data=recogData[recogData$listN!=2,])

summary(modPE)


recogData$PEquant<-relevel(recogData$PEquant, ref="3")
modPE<-glmer(recogAcc~PEquant+(1|SubNum), family=binomial(),
             data=recogData[recogData$listN!=2,])

summary(modPE)




# fistribution of uncertainty
ggplot(recogData, aes (x=uncertainty))+geom_histogram()

ggplot(recogData, aes(uncertainty, recogAcc))+
  geom_smooth(method="glm",formula=y~x,method.args=list(family="binomial"), se=F)+aes(colour = factor(SubNum))+
  geom_smooth(method="glm",formula=y~x,method.args=list(family="binomial"), colour="black", se=T)+
  facet_grid(.~accuracy)+
  theme(strip.text.x = element_text(size = 13))

# is that significant>










# check if the beta or alpha were correlated
parameters<-read.csv("output_files/estimated_parameters.csv")
# assign alpha and beta to the dataframe
recogData$alpha<-NA
recogData$beta<-NA
#

### model it



for (n in 1: nrow(recogData)){
  recogData$alpha[n]<-parameters$alpha[parameters$PartNum==recogData$SubNum[n]]
  recogData$beta[n]<-parameters$beta[parameters$PartNum==recogData$SubNum[n]]
}

# PE by alpha
ggplot(recogData[recogData$listN!=2,], aes(alpha, PE))+
  geom_smooth(method="lm",formula=y~x)+ aes(colour="black", se=T)+
  theme(strip.text.x = element_text(size = 13))

# alpha by beta
ggplot(recogData[recogData$listN!=2,], aes(alpha, beta))+
  geom_smooth(method="lm",formula=y~x)+ aes(colour="black", se=T)+
  theme(strip.text.x = element_text(size = 13))

# allpha by recacc  
ggplot(recogData[recogData$listN!=2,], aes(alpha, recogAcc))+
  geom_smooth(method="lm",formula=y~x)+ aes(colour="black", se=T)+
  theme(strip.text.x = element_text(size = 13))+
facet_wrap(.~befAft)

# alpha by recog accuracy and befAft
ggplot(recogData[recogData$listN!=2,], aes(alpha, recogAcc))+
  geom_smooth(method="lm",formula=y~x)+ aes(colour="black", se=T)+
  theme(strip.text.x = element_text(size = 13))+
  facet_wrap(.~befAft)

# is there an interaction?
modAlphabybefaft<-glmer(recogAcc~alpha*befAft+(1+befAft|SubNum), family=binomial(),
             data=recogData[recogData$listN!=2,])

summary(modAlphabybefaft)

# what about beta?
ggplot(recogData[recogData$listN!=2,], aes(beta, recogAcc))+
  geom_smooth(method="lm",formula=y~x)+ aes(colour="black", se=T)+
  theme(strip.text.x = element_text(size = 13))+
  facet_wrap(.~befAft)


# check if PE explains additional variance
modacc<-glmer(recogAcc~accuracy+(1+ accuracy|SubNum), family=binomial(),
             data=recogData[recogData$listN!=2,])
summary(modacc)

modaccPE<-glmer(recogAcc~accuracy+PE+(1+PE+ accuracy|SubNum), family=binomial(),
                      data=recogData[recogData$listN!=2,])
summary (modaccPE)

