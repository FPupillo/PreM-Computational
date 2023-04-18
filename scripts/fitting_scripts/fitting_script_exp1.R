#------------------------------------------------------------------------------------------------#
# script that fit the winning models to each dataset and print the outputs
#
# this version is for the pilot setup. 
#------------------------------------------------------------------------------------------------#
# fit the models to the data

# set current directory to the parent folder
setwd(dirname(getwd()))

rm(list=ls())
cd<-getwd()
# source the files with the function
source(("computational_model/helper_functions/BICcompute.R"))
source(("computational_model/helper_functions/searchGlobal.R"))
source(("computational_model/helper_functions/softmax.R"))
source(("computational_model/helper_functions/getFiles.R"))
source(("computational_model/helper_functions/modelFunPhase2.R"))
source(("computational_model/helper_functions/getResp.R"))
source(("computational_model/helper_functions/getProbStrongWeak.R"))
source(("computational_model/helper_functions/getx.R"))
source(("computational_model/helper_functions/getU.R"))
source(("computational_model/helper_functions/getQsPhase1.R"))
source(("computational_model/helper_functions/var_murphy.R"))
source(("computational_model/helper_functions/getobs.R"))
source(("computational_model/helper_functions/getfeedb.R"))

# likelihood functions
setwd("computational_model/likelihood_functions")
likfun<-list.files()
for (f in 1:length(likfun)){
  source(likfun[f])
}
# fitting functions
setwd(paste(cd, "/computational_model/fitting_functions",sep=""))
fitfun<-list.files()
for (f in 1:length(fitfun)){
  source(fitfun[f])
}

setwd(cd)

# very important, the setup
setup <-"exp1"

# we have already estimated parameters on file 1 +file2. To fit the models, we can use only
phase1Files<-selPhase(phase = 1, experiment = "exp1")
setwd(cd)

# get phase 2
phase2Files<-selPhase(phase = 2, experiment = "exp1")
setwd(cd)

# retrieve the winning models
mod<-"fLR_Instr"

# select the models of the setup
#winMod<-winMod[winMod$setup==setup,c ("BIC", "LL")]

# unlist
#winMod<-as.character(unlist(winMod))

# loop over mod
#for(mod in winMod){
  
  # load likelihood function
  likelihoodfunction = get(paste("lik_", mod, sep=""))
  
  # load fitting function
  fittingfunction = get(paste("fit_", mod, sep=""))
  
  # getparameters
  filename<-paste("exp1/outputs/group_level/computational_model",
                  "/ParameterEstimation.exp1.betalimit=10.initialQ=0.33.", mod, ".csv", sep="")
  
  # read the file with the parameters
  param<-read.csv(filename)
  
  # create an empty variable that will be the dataset with all participants' data
  dataAllPhase1<-vector()
  
  dataAllPhase2<-vector()
  
  dataAll<-vector()
  # for each subject
  for (j in 1:length(phase1Files)){
    
    
    # we are fitting the models only to phase 2, but we need phase 1 to get the 
    # final estimated values for each scene
    
    # load phase 1 files
    file1<-read.csv(paste("exp1/trial_sequences/",phase1Files[j], sep=""))
    
    # load phase 2 files
    file2<-read.csv(paste("exp1/trial_sequences/",phase2Files[j], sep=""))
    
    # extract subjn
    SubNum<-round(as.numeric(substr(phase2Files[j],5,6)),1)
    
    print(paste("working on Sub= ", SubNum ))
    
    # get parameters
    parameters<-param[param$PartNum==SubNum,]
   
    #---------------------------------------------------------------------------------------------#
    #---------------------------------extract parameters -----------------------------------------#
    #---------------------------------------------------------------------------------------------#
    alpha<-parameters$alpha
    beta<-parameters$beta
    alpha_c<-parameters$alpha_c
    beta_c<-parameters$beta_c
    initialQ =  matrix(0.33, ncol = 3, nrow = 6)
    #---------------------------------------------------------------------------------------------#
    
    #---------------------------------------------------------------------------------------------#
    #---------------------------------extract Qs -------------------------------------------------#
    #---------------------------------------------------------------------------------------------#
    # we need to extract the Qs from phase1 as starting Qs for phase2

      

      
    data1<-likelihoodfunction(Data=file1, alpha=alpha, beta=beta, 
                              print = 2 , initialQ=initialQ)    

    # create subjnum variable
    data1$SubNum<-SubNum
    
    # append to dataframe
    dataAllPhase1<-rbind(dataAllPhase1, data1)
    
    
    # now get the initial Qs for phase2
    initialQ<-getQsPhase1(data1)
    

      fitdata<-likelihoodfunction(Data = file2, alpha = alpha, beta = beta,
                                  print =2, initialQ =  initialQ)
    

    
    # create subjnum variable
    fitdata$SubNum<-SubNum
    
    # bind them with the whole dataset for phase 2
    dataAllPhase2<-rbind(dataAllPhase2, fitdata)
  }
  
  # loop through subjects to get the recognition data
  for (j in 1:length(phase2Files)){
    tryCatch({
    # extract subjn
    SubNum<-round(as.numeric(substr(phase2Files[j],5,6)),1)
    
    # file name 
    if (SubNum<10){
      filename<-paste("PEM_", SubNum,"_results.csv", sep="")
    }else{ 
      filename<-paste("PEM_", SubNum,"_results.csv", sep="")
    }
    
    # read data
    data <- read.csv(paste("exp1/merged_phases/", filename, sep="" ))
    
    # select only old items
    data<-data[data$rec_trialType=="old",]
    
    # append it to the final dataset  
    if (!exists('data3final')){
      data3final<-data
    } else{
      data3final<-rbind(data3final, data)
    }
  
}, error = function(e) { print(paste("problem with number", j))})
}
  # now merge the two datasets (encoding - model and recognition)
  DataAll<-cbind(dataAllPhase2,data3final)
  DataAll$pe_level<-as.factor(DataAll$pe_level)
  DataAll$rec_session<-as.factor(DataAll$rec_session)
  levels(DataAll$pe_level)<-c("LowPE", "MediumPE", "HighPE")
  levels(DataAll$rec_session)<-c("ImmediateRec", "DelayedRec")
  
  # save the data
  write.csv(dataAllPhase1, paste("computational_model/output_files/", "fittedData.", setup, ".Phase1.",mod, 
                           ".csv", sep=""), row.names = F)
  
  write.csv(DataAll, paste("computational_model/output_files/", "fittedData.", setup, ".Phase2.",mod, 
                           ".csv", sep=""), row.names = F)
}


