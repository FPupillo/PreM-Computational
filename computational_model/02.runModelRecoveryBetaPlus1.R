#------------------------------------------------------------------------------#
# Model Recovery
#
#
#"Mon Nov 15 10:57:50 2021"
#
#------------------------------------------------------------------------------#

rm(list=ls())
library(foreach)
library(doParallel)
library(pracma)
library(here)


cd<-getwd()
# source the files with the functions  
source("helper_functions/getmu.R")
source("helper_functions/BICcompute.R")
source("helper_functions/searchGlobal.R")
source(("helper_functions/softmax.R"))
source(("helper_functions/update.R"))
source(("helper_functions/getx.R"))
source(("helper_functions/getobs.R"))
source(("helper_functions/var_murphy.R"))
source(("helper_functions/chooseBinomial.R"))
source(("helper_functions/chooseMultinom.R"))
source(("helper_functions/getSceneCond.R"))
source(("helper_functions/getProbStrongWeak.R"))

# simulation functions
setwd("simulation_functions")
simfun<-list.files()
for (f in 1:length(simfun)){
  source(simfun[f])
}

# likelihood functions
setwd(paste(cd, "/likelihood_functions",sep=""))
likfun<-list.files()
for (f in 1:length(likfun)){
  source(likfun[f])
}
# fitting functions
setwd(paste(cd, "/fitting_functions",sep=""))
fitfun<-list.files()
for (f in 1:length(fitfun)){
  source(fitfun[f])
}

setwd(cd)

# how many simulations?
sims<-100
# how many trials?
trials<-100

# function that takes the arguments from the command line
#Args<-commandArgs(trailingOnly = T)
# for debugging purposes
betalim<-10

# run it for the two experiments
setups<-c("exp1", "exp2")

for(setup in setups){ # loop through experiments

if (setup=="exp1"){
  initialQ<-0.33
} else {
  initialQ<-0.50
}

print(setup)

# what are the contingencies?
mu<-getmu(setup)

# fittings parameters
alphaBound<-c(0,1)
alpha_cBound<-c(0,1)

betaBound<-c(0, betalim)

initialQ <- matrix(initialQ, ncol=ncol(mu), nrow=6)

# make a loop to simulate participants' behaviour for specific values 
# of alpha and beta
alphaseq<-seq(0, 1, length.out = sims)

alpharan<-sample(alphaseq, sims, replace=F)

alphaOran<-sample(alphaseq, sims, replace=F)

# beta is sampled from the exponential distribution.Add one
betaran<-rexp(sims,2/10)+1

# write the file
name<- paste("output_files/modelRecovery.",setup, ".",  
             "betalimit=",  betalim,  
             ".initialQ=", initialQ[1,1] , sep="")

data<-matrix(NA, nrow=3,ncol = 3)

df<-data.frame(data)

models<-c("dLR_Instr", "fLR_Instr", "fLR_Eval")
names(df)<-models
row.names(df)<-names(df)

# progress bar
prb<-txtProgressBar(min=0, max=sims, style=3)

# detect cores for runnning in parallel
cores=detectCores()

cl <- makeCluster(cores[1]-floor(cores/3), outfile="") # to not overload your computer
registerDoParallel(cl)

# export all the functions
functions<-lsf.str()

# loop through several simuolations
#dat<-foreach (j=1:sims, .combine=rbind,.packages=c('pracma', 'here'))  %dopar% {
dat<-foreach (s=1:sims,.export=c(functions))  %dopar% {

  print(paste0("working on sim ", s))
  
  # loop through models
  for (j in 1: length(models)){
  
    mod<-(models[j])
    
    print(paste0("simulating model ", mod))
    
    simulation_function<-get(paste("simulate_", mod, sep=""))
    
    print(paste0("alpha=",alpharan[s], beta = betaran[s] ))
    
    # simulate
    sim<-simulation_function(T = trials, mu =  mu, alpha = alpharan[s], 
                            beta =  betaran[s], initialQ = initialQ )
  
    # change the a to response
    sim$response<-sim$a
    # change the scene to scene cat
    sim$scene_cat<-sim$scene
    # change the scene to scene cat
    sim<-getSceneCond(sim)
    # get accuracy
    sim$acc<-ifelse(sim$response==sim$object_cat, 1,0)

    BEST<-fit_all(data = sim, alphaBound = alphaBound, betaBound = betaBound,
                  initialQ = initialQ)
    
    df[j,]<-BEST
    
    
  }
   
  # print it
  write.csv(df,paste0("temp/ModelRecovery.", setup, ".simN=", s,".csv"))
  
  setTxtProgressBar(prb, j)
  
}

}
stopCluster(cl)
