#------------------------------------------------------------------------------# 
#get the probability of stay depending on previous response
# created [1] "Fri Sep  9 09:32:09 2022"
#------------------------------------------------------------------------------# 
rm(list=ls())
library(ggplot2)
source("computational_model/helper_functions/getPstay.R")
source("computational_model/helper_functions/get_prev_accuracy.R")

# first, get the data
phase2Files<- read.csv2("exp2/outputs/group_level/share/group_task-rec.csv",
                        sep=";", header=T,stringsAsFactors=FALSE)

phase2Files$contingency<-phase2Files$trial_cond

names(phase2Files)[4]<-"trialN"

phase2Files<-phase2Files[!is.na(phase2Files$obj_cat_num),]

phase2Files<-getPstay(phase2Files)

# get the accuracy of the previous trials 
phase2Files<-get_prev_accuracy(phase2Files,1)

# 1 is flat, 2 is weak, and 3 is strong

# Plot prob stay as a funciton of previous response and scene cat
ggplot(phase2Files[!is.na(phase2Files$acc_bef),], aes(acc_bef, stayShift))+
  
  geom_bar(aes(acc_bef, stayShift, fill = acc_bef),
           position="dodge",stat="summary", fun.y="mean", SE=T)+
  
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  
  theme_bw()+
  # geom_line(data = groupData, aes(acc_bef, stayShift,group = participant), size=1, alpha=0.2, stat="summary")+
  facet_grid(.~scn_condition)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab("prob_stay")+
  #scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

#
