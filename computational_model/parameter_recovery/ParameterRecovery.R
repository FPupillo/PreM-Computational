# -----------------------------------------------------------------------------#
# Parameter recovery functions for the Evaluative Model with the decreasing
# learning rate
#
#   1: upper bound for beta--
#   2: starting points
#   3: initialQ
#   4: experiment ("exp1", "exp2")
#   5: Model
# -----------------------------------------------------------------------------#

rm(list=ls())
library(foreach)
library(doParallel)
library(pracma)
library(here)

cd<-getwd()
setwd(cd)

#setwd("computational_model")
# source the files with the functions  
source("helper_functions/getmu.R")
source("helper_functions/BICcompute.R")
source("helper_functions/searchGlobal.R")
source(("helper_functions/softmax.R"))
source(("helper_functions/update.R"))
source(("helper_functions/getx.R"))
source(("helper_functions/getobs.R"))
source(("helper_functions/getSceneCond.R"))
source(("helper_functions/var_murphy.R"))
source(("helper_functions/chooseBinomial.R"))
source(("helper_functions/getProbStrongWeak.R"))

# retrieve the functions
functions<-c( "likelihood_functions", "fitting_functions", "simulation_functions")

for (f in functions){
  # helper functions
  setwd(paste0(f))
  
  fun<-list.files()
  for (f in 1:length(fun)){
    source(fun[f])
  }
  setwd(cd)
  
}

# how many simulations?
sims<-100
# how many trials per scene?
trials<-300

# function that takes the arguments from the command line
Args<-commandArgs(trailingOnly = T)

# for debugging purposes
#Args<-c(10, 1, 0.33, "exp1")

# what are the contingencies?
mu<-getmu(Args[4])

setup<-Args[4]

# fittings parameters
alphaBound<-c(0,1)

if (Args[2]=="infinite"){
  beta2<-"i"}else {beta2<-Args[2]}

betaBound<-c(0, Args[1])

betaBound[2]<-Args[1]

startPoints<-as.numeric(Args[2])

initialQ<-as.numeric(Args[3])

initialQ<-matrix(initialQ, nrow = 6 ,ncol = ncol(mu))

# create matrix and write it down a file
models<-c( "fLR_Instr", "fLR_Eval")

betalim<-10


#setwd("computational_model")
for (model in models){ # loop for the models
  
  name<- paste("output_files/parameterRecovery.",setup, ".", model, ".", 
               "betalimit=",  betalim,  
               ".initialQ=", initialQ[1,1] , sep="")
  
  data<-matrix(NA, nrow=1,ncol = 5)
  
  df<-data.frame(data)
  
  names(df)<-c("simAlpha", "fitAlpha", "simBeta", "fitBeta",  "BIC")
  
  # write it
  write.csv(df, paste0(name, ".csv"), row.names = F)
  
  print(c(betaBound[2],startPoints, initialQ[1]))
  
  # make a loop to simulate participants' behaviour for specific values of alpha and beta
  alphaseq<-seq(0, 1, length.out = sims)
  
  alpharan<-sample(alphaseq, sims, replace=F)
  
  # beta is sampled from the exponential distribution
  betaran<-rexp(sims,2/10)
  
  # progress bar
  prb<-txtProgressBar(min=0, max=sims, style=3)
  
  # detect cores for runnning in parallel
  cores=detectCores()
  
  cl <- makeCluster(cores[1]-floor(cores[1]/3), outfile="") # to not overload your computer
  registerDoParallel(cl)
  
  # get simulation function
  sim_function<-get(paste0("simulate_", model))
  
  # get fitting function
  fit_function<-get(paste0("fit_", model))
  
  
  # loop through several simuolations
  dat<-foreach (j=1:sims, .combine=rbind,.packages=c('pracma', 'here'))  %dopar% {
    
    # simulate data
    sim<-sim_function(T = trials, mu =  mu,alpha = alpharan[j],
                      beta =  betaran[j],
                      initialQ = initialQ)
    
    # change the a to response
    sim$response<-sim$a
    
    sim$scene_cat<-sim$scene
    
    # change the scene to scene cat
    sim<-getSceneCond(sim)
    
    # create accuracy
    sim$acc<-ifelse(sim$response==sim$object_cat,1,0)
    
    # estimate parameters
    est<-searchGlobal(data = sim, alphaBound = alphaBound, betaBound = betaBound, 
                      startPoints = startPoints, initialQ = initialQ, 
                      fittingfunction = fit_function, model = model )   # assign to the dataset
    
    if (!is.null(est$alpha)){
      alpha<-est$alpha
    }else{
      alpha<-NA
    }
    data<-c(alpharan[j], alpha, betaran[j],est$beta,   est$BIC)
    
    #progress bar
    setTxtProgressBar(prb, j) 
    
    # read and write temp data
    temp<-read.csv( paste0(name, ".csv"))
    
    #append the data
    temp[nrow(temp)+1, ]<-data
    
    #write it
    write.csv(temp, paste0(name, ".csv"), row.names = F)
    
  } # end loop models
} # end loop simulations

stopCluster(cl)
