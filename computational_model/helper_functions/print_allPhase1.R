# standalone function to print the files of phase 1

# when running it from terminal, the arguments are
# 1 type of plot: 1:cumulative choice, 2: Actual choices
# 2: beta upper boundary: infinite, 10, 20, 700
# 3: initialQ
library(here)
cd<-getwd()
# source the files with the function
source(here("helper_functions/BICcompute.R"))
source(here("helper_functions/searchGlobal.R"))
source(here("helper_functions/softmax.R"))
source(here("helper_functions/getFiles.R"))
# likelihood functions
setwd("likelihood_functions")
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

models<-c("CK", "RWobs","RWobsALL","RWobsALL1n", "RWfeedb0", "RWfeedb0.33","RWfeedandQ", "RWfeedbCK", "RWfeedbAndObs")

cd<-getwd() # as the following function change the directory, we need to store it and recall it after calling the function
phase1Files<-selPhase(1)
setwd(cd)


# kind of plot. take it from the command line
printplot<-1

# set boundaries for the optimization algorithm
alphaBound<-c(0,1)

betaBound<-c(0,30)

print(betaBound)

initialQ<-as.numeric(0)
# create a name for the output file
# loop through the different models
m<-1
currMod<-models[m]

name<-paste("output_files/plotPhase1.", currMod, sep="")
if (printplot==1){
  pdf(paste(name, ".cumulative.pdf", sep=""))
}else{(paste(name, ".choices.pdf", sep=""))}

#get the files with the parameters
param<-read.csv("output_files/ParameterEstimation.modelCK.csv")

# set the margins for the graphs
par(mfrow=c(3,3))
par(mar = c(3,2,4,1))

for (j in 1:length(phase1Files)){
  try({
  file<-read.csv(paste("trial_sequences/",phase1Files[j], sep=""))
  
  #get alpha, beta
  alpha<-param$alpha[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  beta<-param$beta[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  par<-lik_ChoiceKernel(file,alpha, beta,2,  0)
  source(here("helper_functions/PrintPlot.R"))
  })
}

plot(preference,  typ="n",axes=FALSE,ann=FALSE)
legend("top", inset=0.1,legend="Participant preference %", pch=1)
legend("center", legend=c("Q value", "PE", "Probability"), lty=1,col=c("blue", "red", "green") )
# Close the pdf file
dev.off()
par(mfrow=c(1,1))
setwd(cd)
#rm(list=ls())

m<-2
currMod<-models[m]

name<-paste("output_files/plotPhase1.", currMod, sep="")

if (printplot==1){
  pdf(paste(name, ".cumulative.pdf", sep=""))
}else{(paste(name, ".choices.pdf", sep=""))}

#get the files with the parameters
param<-read.csv("output_files/ParameterEstimation.modelobs.csv")

# set the margins for the graphs
par(mfrow=c(3,3))
par(mar = c(3,2,4,1))
for (j in 1:length(phase1Files)){
  file<-read.csv(paste("trial_sequences/",phase1Files[j], sep=""))
  
  #get alpha, beta
  alpha<-param$alpha[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  beta<-param$beta[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  par<-lik_RescorlaWagner_obs(file,alpha, beta,2,  0)
  source(here("helper_functions/PrintPlot.R"))
}

plot(preference,  typ="n",axes=FALSE,ann=FALSE)
legend("top", inset=0.1,legend="Participant preference %", pch=1)
legend("center", legend=c("Q value", "PE", "Probability"), lty=1,col=c("blue", "red", "green") )
# Close the pdf file
dev.off()
par(mfrow=c(1,1))
setwd(cd)

m<-3
currMod<-models[m]

name<-paste("output_files/plotPhase1.", currMod, sep="")

if (printplot==1){
  pdf(paste(name, ".cumulative.pdf", sep=""))
}else{(paste(name, ".choices.pdf", sep=""))}

#get the files with the parameters
param<-read.csv("output_files/ParameterEstimation.modelObsALL.csv")

# set the margins for the graphs
par(mfrow=c(3,3))
par(mar = c(3,2,4,1))
for (j in 1:length(phase1Files)){
  file<-read.csv(paste("trial_sequences/",phase1Files[j], sep=""))
  
  #get alpha, beta
  alpha<-param$alpha[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  beta<-param$beta[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  par<-lik_RescorlaWagner_obsALLCAT(file,alpha, beta,2,  0)
  source(here("helper_functions/PrintPlot.R"))
}

plot(preference,  typ="n",axes=FALSE,ann=FALSE)
legend("top", inset=0.1,legend="Participant preference %", pch=1)
legend("center", legend=c("Q value", "PE", "Probability"), lty=1,col=c("blue", "red", "green") )
# Close the pdf file
dev.off()
par(mfrow=c(1,1))
setwd(cd)

m<-4
currMod<-models[m]

name<-paste("output_files/plotPhase1.", currMod, sep="")

if (printplot==1){
  pdf(paste(name, ".cumulative.pdf", sep=""))
}else{(paste(name, ".choices.pdf", sep=""))}

#get the files with the parameters
param<-read.csv("output_files/ParameterEstimation.modelObsALL1n.csv")

# set the margins for the graphs
par(mfrow=c(3,3))
par(mar = c(3,2,4,1))
for (j in 1:length(phase1Files)){
  file<-read.csv(paste("trial_sequences/",phase1Files[j], sep=""))
  
  #get alpha, beta
  alpha<-param$alpha[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  beta<-param$beta[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  par<-lik_RescorlaWagner_obsALLCAT(file,alpha, beta,2,  0)
  source(here("helper_functions/PrintPlot.R"))
}

plot(preference,  typ="n",axes=FALSE,ann=FALSE)
legend("top", inset=0.1,legend="Participant preference %", pch=1)
legend("center", legend=c("Q value", "PE", "Probability"), lty=1,col=c("blue", "red", "green") )
# Close the pdf file
dev.off()
par(mfrow=c(1,1))
setwd(cd)

m<-4
currMod<-models[m]

name<-paste("output_files/plotPhase1.", currMod, sep="")

if (printplot==1){
  pdf(paste(name, ".cumulative.pdf", sep=""))
}else{(paste(name, ".choices.pdf", sep=""))}

#get the files with the parameters
param<-read.csv("output_files/ParameterEstimation.modelfeedbRW0.csv")

# set the margins for the graphs
par(mfrow=c(3,3))
par(mar = c(3,2,4,1))
for (j in 1:length(phase1Files)){
  file<-read.csv(paste("trial_sequences/",phase1Files[j], sep=""))
  
  #get alpha, beta
  alpha<-param$alpha[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  beta<-param$beta[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  par<-lik_RescorlaWagner_feedb(file,alpha, beta,2,  0)
  source(here("helper_functions/PrintPlot.R"))
}

plot(preference,  typ="n",axes=FALSE,ann=FALSE)
legend("top", inset=0.1,legend="Participant preference %", pch=1)
legend("center", legend=c("Q value", "PE", "Probability"), lty=1,col=c("blue", "red", "green") )
# Close the pdf file
dev.off()
par(mfrow=c(1,1))
setwd(cd)

m<-5
currMod<-models[m]

name<-paste("output_files/plotPhase1.", currMod, sep="")

if (printplot==1){
  pdf(paste(name, ".cumulative.pdf", sep=""))
}else{(paste(name, ".choices.pdf", sep=""))}

#get the files with the parameters
param<-read.csv("output_files/ParameterEstimation.modelfeedbRW0.3.csv")

# set the margins for the graphs
par(mfrow=c(3,3))
par(mar = c(3,2,4,1))
for (j in 1:length(phase1Files)){
  file<-read.csv(paste("trial_sequences/",phase1Files[j], sep=""))
  
  #get alpha, beta
  alpha<-param$alpha[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  beta<-param$beta[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  par<-lik_RescorlaWagner_feedb(file,alpha, beta,2,  0.33)
  source(here("helper_functions/PrintPlot.R"))
}

plot(preference,  typ="n",axes=FALSE,ann=FALSE)
legend("top", inset=0.1,legend="Participant preference %", pch=1)
legend("center", legend=c("Q value", "PE", "Probability"), lty=1,col=c("blue", "red", "green") )
# Close the pdf file
dev.off()
par(mfrow=c(1,1))
setwd(cd)

m<-6
currMod<-models[m]

name<-paste("output_files/plotPhase1.", currMod, sep="")

if (printplot==1){
  pdf(paste(name, ".cumulative.pdf", sep=""))
}else{(paste(name, ".choices.pdf", sep=""))}

#get the files with the parameters
param<-read.csv("output_files/ParameterEstimation.modelfeedbRWQ.csv")

# set the margins for the graphs
par(mfrow=c(3,3))
par(mar = c(3,2,4,1))
for (j in 1:length(phase1Files)){
  file<-read.csv(paste("trial_sequences/",phase1Files[j], sep=""))
  
  #get alpha, beta
  alpha<-param$alpha[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  beta<-param$beta[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  inQ<-param$Q[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  par<-lik_RescorlaWagner_feedb(file,alpha, beta,2,  inQ)
  source(here("helper_functions/PrintPlot.R"))
}

plot(preference,  typ="n",axes=FALSE,ann=FALSE)
legend("top", inset=0.1,legend="Participant preference %", pch=1)
legend("center", legend=c("Q value", "PE", "Probability"), lty=1,col=c("blue", "red", "green") )
# Close the pdf file
dev.off()
par(mfrow=c(1,1))
setwd(cd)

m<-7
currMod<-models[m]

name<-paste("output_files/plotPhase1.", currMod, sep="")

if (printplot==1){
  pdf(paste(name, ".cumulative.pdf", sep=""))
}else{(paste(name, ".choices.pdf", sep=""))}

#get the files with the parameters
param<-read.csv("output_files/ParameterEstimation.modelfeedbRWCK.csv")

# set the margins for the graphs
par(mfrow=c(3,3))
par(mar = c(3,2,4,1))
for (j in 1:length(phase1Files)){
  file<-read.csv(paste("trial_sequences/",phase1Files[j], sep=""))
  
  #get alpha, beta
  alpha<-param$alpha[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  beta<-param$beta[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  alpha_c<-param$alphac[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  beta_c<-param$betac[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  
  par<-lik_RescorlaWagner_feedbCK(file,alpha, beta,alpha_c, beta_c, 2,  0,0)
  source(here("helper_functions/PrintPlot.R"))
}

plot(preference,  typ="n",axes=FALSE,ann=FALSE)
legend("top", inset=0.1,legend="Participant preference %", pch=1)
legend("center", legend=c("Q value", "PE", "Probability"), lty=1,col=c("blue", "red", "green") )
# Close the pdf file
dev.off()
par(mfrow=c(1,1))
setwd(cd)

m<-8
currMod<-models[m]

name<-paste("output_files/plotPhase1.", currMod, sep="")

if (printplot==1){
  pdf(paste(name, ".cumulative.pdf", sep=""))
}else{(paste(name, ".choices.pdf", sep=""))}

#get the files with the parameters
param<-read.csv("output_files/ParameterEstimation.modelfeedbRWObs.csv")

# set the margins for the graphs
par(mfrow=c(3,3))
par(mar = c(3,2,4,1))
for (j in 1:length(phase1Files)){
  file<-read.csv(paste("trial_sequences/",phase1Files[j], sep=""))
  
  #get alpha, beta
  alpha<-param$alpha[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  beta<-param$beta[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  alphao<-param$alphao[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  betao<-param$betao[param$PartNum==round(as.numeric(substr(phase1Files[j],5,6)),1)]
  
  par<-lik_RescorlaWagner_feedbAndObs(file,alpha, beta,alphao, betao, 2,  0,0)
  source(here("helper_functions/PrintPlot.R"))
}

plot(preference,  typ="n",axes=FALSE,ann=FALSE)
legend("top", inset=0.1,legend="Participant preference %", pch=1)
legend("center", legend=c("Q value", "PE", "Probability"), lty=1,col=c("blue", "red", "green") )
# Close the pdf file
dev.off()
par(mfrow=c(1,1))
setwd(cd)

