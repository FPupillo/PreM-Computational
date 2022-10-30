#------------------------------------------------------------------------------#
# estimate parameter by participant for experiment 1
# last modified: "Fri Nov 26 19:43:15 2021"
#------------------------------------------------------------------------------#
# source the files with the function  
rm(list=ls())
library(foreach)
library(doParallel)
library(beepr)

# set current directory to the parent folder
setwd(dirname(getwd()))

cd<-getwd()
# source the files with the function
source(("computational_model/helper_functions/BICcompute.R"))
source(("computational_model/helper_functions/searchGlobal.R"))
source(("computational_model/helper_functions/softmax.R"))
source(("computational_model/helper_functions/getFiles.R"))
source(("computational_model/helper_functions/modelFunPhase2.R"))
source(("computational_model/helper_functions/getx.R"))
source(("computational_model/helper_functions/getobs.R"))
source(("computational_model/helper_functions/getU.R"))
source(("computational_model/helper_functions/update.R"))
source(("computational_model/helper_functions/getProbStrongWeak.R"))
source(("computational_model/helper_functions/var_murphy.R"))
source(("computational_model/helper_functions/getfeedb.R"))
source(("computational_model/helper_functions/getQsPhase1.R"))
source(("computational_model/helper_functions/fixnames.R"))


setwd(cd)
# likelihood functions
setwd("computational_model/likelihood_functions")
likfun<-list.files()
for (f in 1:length(likfun)){
  source(likfun[f])
}
setwd(cd)
# fitting functions
setwd("computational_model/fitting_functions")

fitfun<-list.files()
for (f in 1:length(fitfun)){
  source(fitfun[f])
}
setwd(cd)

setup <-"exp2"

phase1Files<- read.csv("exp2/outputs/group_level/group_task-learning.csv")

phase2Files<- read.csv2("exp2/outputs/group_level/share/group_task-rec.csv",
                        sep=";", header=T,stringsAsFactors=FALSE)
setwd(cd)


Args<-commandArgs(trailingOnly = T) 
# the arguments are : 
#   1.starting point
#   2.upper beta bound
#   3.initial Q
#   4.modeltype


#Args<-c(15, 10 ,  0.5,"fLR_Instr")


print(Args)

# set the number of the starting points for the optimization function
startPoints<-as.numeric(Args[1])

# set boundaries for the optimization algorithm
alphaBound<-c(0,1)


# for the beta, take the 2 one from the command line
if (Args[2]=="infinite"){
  beta2<-"i"}else {beta2<-as.numeric(Args[2])}

betaBound<-c(0,beta2)

# values at which Q is initialised
initialQ <- matrix(as.numeric(Args[3]), ncol=2, nrow=6)

# Type of model
modeltype = Args[4]

# create a name for the output file
name<-paste("exp2/outputs/group_level/computational_model/ParameterEstimation.exp2_encoding.", 
            "betalimit=",betaBound[2],".initialQ=",  Args[3], ".", modeltype, sep="")


print(Args)

# assign fitting and likelihood functions
fittingfunction = get( paste("fit_", modeltype, sep="" ))

likelihoodfunction = get(paste("lik_", modeltype,  sep=""))


# initialize matrix to store the parameters
Parameters<-matrix(NA, nrow = length(phase1Files),ncol = 5) 
colnames(Parameters)<-c("PartNum", "alpha","beta",  "BIC", "LogLikel") #names of the columns

# get the phase 1 files
phase1File<-read.csv("computational_model/output_files/fittedData_learning.exp2.Phase1.fLR_Instr.csv")

participants<-unique(phase1Files$participant)

# getparameters of the file 1
filename<-paste("exp2/outputs/group_level/computational_model",
                "/ParameterEstimation.exp2_learn.betalimit=10.initialQ=0.5.", modeltype, ".csv", sep="")

# read the file with the parameters
param<-read.csv(filename)

# loop through participants
# make it in parallel
cores=detectCores()
cl <- makeCluster(cores[1]-floor(cores[1]/3), outfile="") # to not overload your computer
registerDoParallel(cl)

Param<-foreach (j=1:length(participants),.combine=rbind, .packages='pracma')  %dopar% {
  #Param<-foreach (j=1:2,.combine=rbind, .packages='pracma')  %dopar% {
  
  tryCatch({
    
    print(paste("Working on participant", participants[j]))
    #counter for the column number of the Matrix that stores the variables
    
    # read the files
    file1<-phase1Files[phase1Files$participant==participants[j],]
    
    # change the names
    names(file1)[c(1:6)]<-c("X.1", "X", "participant.x","trialN" ,"contingency" ,"scn_file.x")  
    
    # subset phase2 files for that participant
    file2<-phase2Files[phase2Files$participant==participants[j],]
    
    file2$contingency<-file2$trial_cond
    
    names(file2)[4]<-"trialN"
    
    # select variables of intrest from the variables names in phase1
    VoI<-c(names(file1)[c(3:12, 14:17, 19)])
    
    
    file2<-file2[,VoI]
    
    file1<-file1[,VoI]
    
    # fix the names to make them match the script
    file1<-fixnames(file1)
    file2<-fixnames(file2)
    
    # add trial acc
    file1$acc<-file1$trial_acc
    file2$acc<-file2$trial_acc
    
    # delete NAs
    file2<-file2[!is.na(file2$scene_cat),]
    
    # name of the model
    model<-modeltype
    
    SubNum<- participants[j]
    
    #---------------------------------------------------------------------------------------------#
    #---------------------------------extract parameters -----------------------------------------#
    #---------------------------------------------------------------------------------------------#
    # get parameters for participant
    parameters<-param[param$PartNum==SubNum,]
    
    
    alpha<-parameters$alpha
    beta<-parameters$beta
    alpha_c<-parameters$alpha_c
    beta_c<-parameters$beta_c
    #---------------------------------------------------------------------------------------------#
    
    # extract the Q
    data1<-likelihoodfunction(Data=file1, alpha=alpha, beta=beta, 
                              print = 2 , initialQ=initialQ)    
    
 
    # get he Q
    initialQ<-getQsPhase1(data1)
    
    # estimate alpha and beta, calculating the time
    start_time<-Sys.time()
    est<-searchGlobal(data = file2,alphaBound= alphaBound,betaBound= betaBound, 
                      startPoints=  startPoints,initialQ= initialQ, 
                      fittingfunction =fittingfunction, 
                      model = model)
    
    #est<-likelihoodfunction(data =file,  alphaBound,betaBound= betaBound, omegaBound = )
    end_time<-Sys.time()
    print(end_time-start_time)
    
    if (is.null(est$alpha)){# this is for the model 1/n where the alpha is null.
      alpha<-NA
    } else{
      alpha<-est$alpha
    }
    
    beta<-est$beta
    BIC<-est$BIC
    LL<-est$logLikel
    
    
  }, error = function(e) { print(paste("problem with number", j,
                                       "ERROR:", conditionMessage(e)))})
  
  paramet<-c("alpha", "beta",  "LL", "BIC")
  
  for (p in paramet){
    if (!exists(p, inherits = F)){
      print(p)
      assign((p),NA)
    }
  }
  
  Parameters<-c(SubNum,
                alpha, beta, BIC, LL)
  Parameters
  
  
  
}


colnames(Param)<-c("PartNum", "alpha","beta", "BIC", "LogLikel") #names of the columns

dataframe<-as.data.frame(Param)


write.csv(dataframe, paste(name, ".csv", sep=""), row.names = F)


stopCluster(cl)

beep(8)
