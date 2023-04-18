#------------------------------------------------------------------------------#
# estimate parameter by participant for experiment 1
# last modified: "Fri Nov 26 19:43:15 2021"
#------------------------------------------------------------------------------#
# source the files with the function  
rm(list=ls())
library(here)
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

# very important, the setup
setup <-"exp1"

# select only phase 1
phase1Files<-selPhase(phase = 1, experiment = "exp1")
setwd(cd)

# get phase 2
phase2Files<-selPhase(phase = 2, experiment = "exp1")
setwd(cd)


Args<-commandArgs(trailingOnly = T) 
# the arguments are : 
#   1.starting point
#   2.upper beta bound
#   3.initial Q
#   4.modeltype


#Args<-c("y", 10 ,  0.33,"dLR_Instr")


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
initialQ <- matrix(as.numeric(Args[3]), ncol=3, nrow=6)

# Type of model
modeltype = Args[4]

# create a name for the output file
name<-paste("exp1/outputs/group_level/computational_model/ParameterEstimation.exp1.", 
            "betalimit=",betaBound[2],".initialQ=",  Args[3], ".", modeltype, sep="")


print(Args)

# assign fitting and likelihood functions
fittingfunction = get( paste("fit_", modeltype, sep="" ))

likelihoodfunction = get(paste("lik_", modeltype,  sep=""))


# initialize matrix to store the parameters
Parameters<-matrix(NA, nrow = length(phase1Files),ncol = 5) 
colnames(Parameters)<-c("PartNum", "alpha","beta",  "BIC", "LogLikel") #names of the columns

# loop through participants
# make it in parallel
cores=detectCores()
cl <- makeCluster(cores[1]-floor(cores[1]/3), outfile="") # to not overload your computer
registerDoParallel(cl)

Param<-foreach (j=1:length(phase1Files),.combine=rbind, .packages='pracma')  %dopar% {
  #Param<-foreach (j=1:2,.combine=rbind, .packages='pracma')  %dopar% {
  
  tryCatch({
    
    print(paste("Working on participant", j))
    #counter for the column number of the Matrix that stores the variables
    
    # read the files
    file1<-read.csv(paste("exp1/trial_sequences/",phase1Files[j], sep=""))
    
    file2<-read.csv(paste("exp1/trial_sequences/",phase2Files[j], sep=""))
    
    # merge them
    file<-rbind(file1, file2)
    
    # name of the model
    model<-modeltype
    
    # estimate alpha and beta, calculating the time
    start_time<-Sys.time()
    est<-searchGlobal(data = file,alphaBound= alphaBound,betaBound= betaBound, 
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
  
  Parameters<-c(round(as.numeric(substr(phase1Files[j],5,6)),1),
                alpha, beta, BIC, LL)
  Parameters
  
  
  
}


colnames(Param)<-c("PartNum", "alpha","beta", "BIC", "LogLikel") #names of the columns

dataframe<-as.data.frame(Param)


write.csv(dataframe, paste(name, ".csv", sep=""), row.names = F)


stopCluster(cl)

beep(8)
