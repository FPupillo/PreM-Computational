# script to plot difference betweeen local-global as a function of iteration 
# through different starting points
library(foreach)
library(doParallel)
# source the files with the function
source("01.getFiles.R")
source("02.modelFun.R")
source("03.fittingFun.R")
# select only phase 1
cd<-getwd() # as the following function change the directory, we need to store it and recall it after callin ght function
phase1Files<-selPhase(1)
setwd(cd)
#parameter boundaries
alphaBound<-c(0.01,1)
betaBound<-c(0.1, 700)

# starting points
startPoints<-floor(seq(1, 10, length.out = 10))
feedback<-1
# initialize matrix to store the parameters
Parameters<-matrix(NA, nrow = length(phase1Files),ncol =length(startPoints) ) 
colnames(Parameters)<-startPoints

# loop through participants
# make it in parallel
cores=detectCores()
cl <- makeCluster(cores[1]-1, outfile="") # to not overload your computer
registerDoParallel(cl)

Param<-foreach (j=1:2,.combine=rbind)  %dopar% {#  length(phase1Files), .combine=rbind)  %dopar% {
  print(paste("Working on participant", j))
  #counter for the column number of the Matrix that stores the variables
  
  # read the files
  file<-read.csv(paste("trial_sequences/",phase1Files[j], sep=""))
  # initiallize variable
  diffALL<-NA
# loop over starting points
  for (s in 1:length(startPoints)){

localAlpha<-fit_M3RescorlaWagner_v1(file,alphaBound, betaBound, feedback)$alphabetaPAR[1]
globalApha<-searchGlobal(file, alphaBound, betaBound, startPoints[s], feedback)$alpha

alphadiff<-abs(globalApha-localAlpha)

diffALL[s]<-alphadiff}
Parameters<-diffALL
Parameters
  }


# average per column
localglob<-colMeans(Param, na.rm=T)
sdlocalglob<-apply(Param, 2, sd, na.rm=T)

# plot it
name<-paste("output files/StartingPoints", ".beta=", betaBound[1], ",", betaBound[2], ".feedback=", feedback, sep="")
write.csv(paste(name, ".csv", sep=""))
pdf(paste(name, ".pdf", sep=""))
plot(startPoints, localglob, pch=19,xlab="Starting Point", ylab="local/global difference")
arrows(startPoints, localglob-sdlocalglob, startPoints, localglob+sdlocalglob,length=0.05, angle=90, code=3)
dev.off()

