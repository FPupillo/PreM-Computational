
selPhase<- function(phase, experiment){
  # This function extracts the names of the files that belongs to either phase 1 or phase 2
  #
  # Input: Phase
  #   1: select files of Phase 1
  #   2: select files of Phase 2
  #       experiment
  #
  # Output:
  #   A string file containing the names of the files referring to the phase selected (1,2)
  # ---------------------
  
  # get current directory to reset the path later, depending on the argument
  if (experiment == "exp1"){
  setwd("exp1/trial_sequences/")
  } 
  
  phase1Files<-NULL
  phase2Files<-NULL
  for (i in 1: length (list.files())){
    check<-substr((list.files()[i]), 8,13)
    if(check=="phase1"){
      phase1Files<- c(phase1Files,(list.files()[i]))
    }
  } 
  phase2files<-NULL
  for (i in 1: length (list.files())){
    check<-substr((list.files()[i]), 8,13)
    if(check=="phase2"){
      phase2Files<- c(phase2Files,(list.files()[i]))
    }
  }
  if (phase==1){return(phase1Files)}else{return(phase2Files)}
}