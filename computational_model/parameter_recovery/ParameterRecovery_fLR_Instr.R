# -----------------------------------------------------------------------------#
# Parameter recovery functions for the Instructive Model with a free learning
# rate
#
#   1: upper bound for beta--
#   2: starting points
#   3: initialQ
#   4: experiment ("exp1", "exp2")
# -----------------------------------------------------------------------------#

rm(list=ls())
library(foreach)
library(doParallel)
library(pracma)
#library(here)

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
source(("likelihood_functions/lik_RW_obsALL.R"))
source(("fitting_functions/fit_RW_obsALL.R"))
source(("simulation_functions/simulate_RW_obsALL.R"))

# how many simulations?
sims<-100
# how many trials?
trials<-300

# function that takes the arguments from the command line
Args<-commandArgs(trailingOnly = T)

# for debugging purposes
#Args<-c(10, 1, 0.50, "exp2")

# what are the contingencies?# what are the contingencies?
mu<-getmu(Args[4])

setup <- Args[4]
# fittings parameters
alphaBound<-c(0,1)

if (Args[1]=="infinite"){
  beta2<-'i'}else {beta2<-Args[1]}

betaBound<-c(0,beta2)

betaBound[2]<-beta2

startPoints<-as.numeric(Args[2])

initialQ<-as.numeric(Args[3])

initialQ<-matrix(initialQ, nrow = 6 ,ncol = ncol(mu))

# create matrix and write it down a file
model<-"obsALL"

betalim<-10

name<- paste("output_files/parameterRecovery.",setup, ".", model, ".", 
             "betalimit=",  betalim,  
             ".initialQ=", initialQ[1,1] , sep="")

data<-matrix(NA, nrow=1,ncol = 5)

df<-data.frame(data)

names(df)<-c("simAlpha", "fitAlpha",
             "simBeta", "fitBeta",  "BIC")

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

# loop through several simuolations
dat<-foreach (j=1:sims, .combine=rbind,.packages=c('pracma', 'here'))  %dopar% {
  # simulate data
  sim<-simulate_RW_obsALL(T = trials, mu =  mu, alpha =  alpharan[j],
                       beta = betaran[j], initialQ = initialQ)
  
  # change the a to response
  sim$response<-sim$a
  
  sim$scene_cat<-sim$scene
  # change the scene to scene cat
  sim<-getSceneCond(sim)  
  # estimate parameters
  est<-searchGlobal(data = sim, alphaBound = alphaBound, betaBound = betaBound, 
                    startPoints = startPoints, initialQ = initialQ, 
                    fittingfunction = fit_RescorlaWagner_obsALL , model = "obs" )  
  
  data<-c(alpharan[j], est$alpha, betaran[j],est$beta, est$BIC)
  #progress bar
  setTxtProgressBar(prb, j) 
  
  # read and write temp data
  temp<-read.csv( paste0(name, ".csv"))
  
  #append the data
  temp[nrow(temp)+1, ]<-data
  
  #write it
  write.csv(temp, paste0(name, ".csv"), row.names = F)
  
}

# data<-data.frame(dat)
# names(data)<-c("SimAlpha", "fitAlpha", "simBeta", "fitBeta", "BIC")
# model<-"Obs"
# name<- paste("output_files/parameterRecoveryRW", model, ".setup =", setup, ".betalimit=",  betaBound[2],  
#              ".initialQ=", initialQ[1] , sep="")
# 
# write.csv(data, paste(name, ".csv", sep=""))
# 
# #############################plot!
# 
# # plot alpha first
# jpeg(paste("figures/",  "parameterRecovery", model,".setup =", setup, ".Alpha.",  betaBound[2],  ".initialQ=", initialQ[1], sep=""))
# 
# plot(data$SimAlpha,data$fitAlpha, pch=19,main="Alpha parameter", sub ="red dot for r < 0.25",
#      xlab = "simulated Alpha", ylab = "fit Alpha")
# 
# # mark the bad fits in red
# thresh <- 0.25;
# ind <-abs(data$SimAlpha - data$fitAlpha) > thresh;
# points(data$SimAlpha[ind],data$fitAlpha[ind], pch=19, col="red")
# #abline(lm(data$fitAlpha[!ind]~data$SimAlpha[!ind]))
# abline(lm(data$fitAlpha~data$SimAlpha), col = "green", b=5)
# 
# dev.off()
# 
# # now beta
# jpeg(paste("figures/",  "parameterRecovery", model,".setup =", setup, ".Beta.",  betaBound[2],  ".initialQ=", initialQ[1], sep=""))
# 
# plot(data$simBeta,data$fitBeta, pch=19,main="Beta parameter",
#      xlab = "simulated Beta", ylab = "fit Beta")
# 
# abline(lm(data$fitBeta~data$simBeta))
# 
# aty <- axTicks(1)
# 
# axis(1,at=aty,labels=aty)
# 
# dev.off()

stopCluster(cl)
