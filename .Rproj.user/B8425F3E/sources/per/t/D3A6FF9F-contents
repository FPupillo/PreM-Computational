# -----------------------------------------------------------------------------#
# Script that shows participants' learning during the learning phase and 
# the encoding phase
# 
# ""Fri Nov 26 15:53:07 2021"
# -----------------------------------------------------------------------------#

rm(list=ls())

library(dplyr)
library(ggplot2)
library(here)
library(gridExtra) # for plotting



# retrieve functions
cd<-getwd()
setwd("computational_model")
source("helper_functions/cumAccbyScene.R")
source("helper_functions/getFiles.R")
source("helper_functions/getcumAcc.R")
source("helper_functions/cumAcc.R")

exps<-c("exp1", "exp2")

# number of participants for the two experiments, respectively
participantN<-c(32, 80) 

for (e in 1:length(exps)){
  
# current exp
exp<-exps[e]

# number of participants
participants<-participantN[e]

if (exp == "exp1"){
  setwd(cd)
  
  files_1<-selPhase(1, "exp1")
  setwd(cd)
  
  files_2<-selPhase(2, "exp1")
  setwd(cd)
}

# -----------------------------------------------------------------------------#
# retrieve participants' data


# Initialize a variable for storing all participants' data
partAll<-vector()

for (j in 1:participants){
  tryCatch({
    if (exp =="exp1"){
      
      # get the file of the participant for both the phase1 and phase2
      currfile1<-read.csv(paste("exp1/trial_sequences/", 
                                files_1[j], sep=""))
      currfile2<-read.csv(paste("exp1/trial_sequences/", 
                                files_2[j], sep=""))
    } else {
      
      j<-ifelse(j<10, paste0("0",j), j)
      
      currfile1<- 
        read.csv(paste0("exp2/data/BIDS/sub-0",j,
                        "/sub-0",j,"_task-learning_cleaned.csv"))
      
      currfile2<- 
        read.csv(paste0("exp2/data/BIDS/sub-0",j,
                        "/sub-0",j,"_task-enc_cleaned.csv"))
      
      #fix the names
      names(currfile1)[3]<-"trialN"
      
      # delete unnecessary variables
      currfile1<-subset(currfile1,select=-iteration_index)
      currfile2<-subset(currfile2,select=-fillers)
      
      names(currfile1)[which(names(currfile1)=="scn_cat")]<-"scene_cat"
      
      names(currfile2)[which(names(currfile2)=="scn_cat")]<-"scene_cat"
      
    }
    
    # mark the type as learning or encoding phase
    currfile1$type<-"learning"
    currfile2$type<-"encoding"
    
    # bind them
    currfile<-rbind(currfile1, currfile2)    
    
    # order it
    currfile<-currfile[order(currfile$scene_cat),]
    
    # get the name of the variable that indicates prediciton accuracy, 
    # which is different in exp1 vs exp2
    predAcc<-ifelse(exp=="exp1", "acc", "trial_acc")
    # get cumulative accuracy by scene
    currfile<-cumAccbyScene(currfile, predAcc)
    
    # add participant number
    if (exp =="exp1"){
      if (as.numeric(substr(files_1[j], 5,6))<10){
        currfile$participant<-rep(as.numeric(substr(files_1[j], 6,6)),
                                  nrow(currfile))
      }else{
        currfile$participant<-rep(as.numeric(substr(files_1[j], 5,6)),
                                  nrow(currfile))
      }
    }
 
       # append to the dataframe
    partAll<-rbind(partAll, currfile)
    
  },   error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
}

if (exp=="exp1"){
  # get the scene condition by pe level
  partAll$scn_condition<-ifelse(partAll$pe_level==1  | partAll$pe_level==3, 
                                 "strong", "flat")
} else{
  partAll$scn_condition<-ifelse(partAll$scn_condition==1 , "flat", ifelse(
    partAll$scn_condition==2, "weak", "strong"))
}

# select only the first 40 particiapnts (immediate)
partAll<-partAll[partAll$participant<41,]

# get the standard error and cumulative accuracy for simALL
Datawidepart<- partAll %>%
  group_by( trialNbyscene, scn_condition) %>%
  dplyr::summarise(mean = mean(cumAccbyScene), sd = sd(cumAccbyScene))

Datawidepart$se<-Datawidepart$sd/sqrt(participants)

# get label for plot
label<-ifelse(exp=="exp1", "(a)", "(b)")

assign(paste0("plot", e),
ggplot(Datawidepart, aes(x = trialNbyscene, y=mean, 
  color = scn_condition, fill = scn_condition))+  
  stat_summary(fun.y="mean",geom="line")+ylim(c(0,1))+
  geom_ribbon(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), alpha=0.2, colour=NA)+
  theme_light()+
  xlab("Trial Number by Condition")+
  ylab("Cumulative Accuracy")+
  annotate(geom="text",  label=label,size=8,family="serif")

)

# do the same by ID
DatawidepartID<- partAll %>%
  group_by( trialNbyscene, scn_condition, participant) %>%
  dplyr::summarise(mean = mean(cumAccbyScene), sd = sd(cumAccbyScene))

assign(paste0("plotID", e),
       ggplot(DatawidepartID, aes(x = trialNbyscene, y=mean, 
                                color = scn_condition, fill = scn_condition))+  
         geom_line()+

         #stat_summary(fun.y="mean",geom="line")+ylim(c(0,1))+
         #geom_ribbon(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), alpha=0.2, colour=NA)+
         theme_light()+
         xlab("Trial Number by Condition")+
         ylab("Cumulative Accuracy")+
         #annotate(geom="text",  label=label,size=8,family="serif")+
         facet_wrap(.~participant)
       
)


}

ggpubr::ggarrange( plot1, plot2, ncol=2, labels = c("a)", "b)"))

ggsave(("computational_model/figures/cumAccbySceneAll.jpg"))

# plot the graphs by ID
ggsave(filename = "computational_model/figures/cumAccbySceneExp1byID.jpg", 
       plot=plotID1)
ggsave(filename = "computational_model/figures/cumAccbySceneExp2byID.jpg", 
       plot=plotID2)
