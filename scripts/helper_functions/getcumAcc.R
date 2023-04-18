# get cumulative accuracy over the last 10 congruent trials and returns participants
# who don't reach the performance


getCumacc<-function(data){
  
  #####################################################
  # function that caclulates the cumulative accuracy
  # over the last 8 congruent trials for each scene
  # for Phase 1, and returns the participant number
  # for those that have less than 0.5
  # --------------------------------------------------
  
  library(dplyr)
  source("helper_functions/getFiles.R")
  
  cd<-getwd() # as the following function change the directory, we need to store it and recall it after callin ght function
  phase1Files<-selPhase(1)
  setwd(cd)
  
  partn<-length(phase1Files)
  
  # initialize the file
  param<-data.frame(matrix(NA, nrow=partn, ncol = 2) )

  names(param)<-c("participant", "perfomance10")
  
  # loop through participants
  for (j in 1:length(phase1Files)){
    
    file<-read.csv(paste("trial_sequences/",phase1Files[j], sep=""))
    
    # subset scenes 4,5,6
    fileSub<-file[file$scene_cat==4 | file$scene_cat==5 | file$scene_cat==6,]
    
    # create cumulative accuracy only for congruent trials. they are 16 by scene 
    cumacc<-cumAcc(fileSub$acc[fileSub$pe_level==1])
    
    # select the last 8 trials per scene
    cumacc<-tail(cumacc, 24)
    
    # mean of the last 30 trials, 10 per scene
    param$perfomance10[j]<-mean(cumacc, na.r=T)
    
    # participant
    param$participant[j]<-((substr(phase1Files[j],5,6)))
  }

  exclusion<-param$participant[param$perfomance10<0.50]  

  return(exclusion)  
}