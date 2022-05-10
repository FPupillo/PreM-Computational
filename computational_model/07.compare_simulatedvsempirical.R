# -----------------------------------------------------------------------------#
# script to simulate files according to the best model and compare it to 
# participants' actual data: Phase 2
# "Wed Nov 17 12:43:02 2021"
# -----------------------------------------------------------------------------#

rm(list=ls())

library(dplyr)
library(ggplot2)
library(here)
library(viridis)

# retrieve functions
cd<-getwd()
setwd("computational_model")
source(("helper_functions/BICcompute.R"))
source(("helper_functions/searchGlobal.R"))
source(("helper_functions/softmax.R"))
source("helper_functions/cumAccbyScene.R")
source("helper_functions/getFiles.R")
source("helper_functions/getcumAcc.R")
source("helper_functions/cumAcc.R")
source("helper_functions/update.R")

setwd("simulation_functions")
simfun<-list.files()
for (f in 1:length(simfun)){
  source(simfun[f])
}

setwd(cd)
# likelihood functions
setwd(paste("computational_model/likelihood_functions",sep=""))
likfun<-list.files()
for (f in 1:length(likfun)){
  source(likfun[f])
}

# fitting functions
setwd(paste(cd,"/computational_model/fitting_functions",sep=""))
fitfun<-list.files()

for (f in 1:length(fitfun)){
  source(fitfun[f])
}

setwd(cd)


setups<-c("exp1", "exp2")

# -----------------------------------------------------------------------------#
# loop by setup
for (setup in (setups)){
  
#setup<-"exp1"

# list all the output files that start with "phase1"
setwd(paste(cd,"/", setup, 
            "/outputs/group_level/computational_model", sep=""))

files1<-list.files()


setwd(cd)

# participants
participants<-nrow(read.csv(paste0(cd, "/", setup, 
                                   "/outputs/group_level/computational_model/",
                                   files1[1])))
if (setup=="exp2"){
# select only immediate rec
participants<-40
}

if (setup =="exp1"){
initialQ<-0.33
strong1<-c(0.80, 0.10, 0.10)
strong2<-c(0.10, 0.80, 0.10)
strong3<-c(0.10, 0.10, 0.80)
flat<-c(0.33, 0.33, 0.33)

mu<-matrix(rbind(strong1, strong2, strong3, flat, flat, flat), nrow = 6, ncol =3)
trials<-20+80 # 20 for phase1 and 80 for phase2 

} else {
  initialQ<-0.5
  strong1<-c(0.90, 0.10)
  strong2<-c(0.10, 0.90)
  weak1<-c(0.70, 0.30)
  weak2<-c(0.30, 0.70)
  flat1<-c(0.50, 0.50)
  flat2<-c(0.50, 0.50)
  mu<-matrix(rbind(strong1, strong2, weak1, weak2, flat1, flat2), nrow = 6,
             ncol =2)
  trials<-c(260)
}

# exclude participants that had less than 0.50 cumulative accuracy 
# over the last 8 congruent trials
# of strong prior condition
#exclusions<-getCumacc(files1[1])


#set.seed(134657)
#set.seed(13465791)
# loop through the files

# -----------------------------------------------------------------------------#
# loop by model
for (f in 1:4){
  
  param<-read.csv(paste0(cd, "/", setup, 
                        "/outputs/group_level/computational_model/",
                        files1[f], sep=""))
  
  if (setup=="exp2"){
    # select only immediate recog
    param<-param[param$PartNum<41,]
  }
  
  # get the beta limit
  betalimit<-as.numeric(substr(sub(".*betalimit=", "", files1[3]), 1, 2))
  
  initialQ<-matrix(initialQ, ncol=ncol(mu), nrow = 6)
  
  # get parameters
  alphaMean<-mean(param$alpha)
  alphaSD<-sd(param$alpha)
  betaMean<-mean(param$beta)
  
  betaSD<-sd(param$beta)
  
  # simulate the data using the parameters estimated by the model
  # first, create a dataframe without participants excluded
  #exclusions<-c(2, 7, 16,32)
  
  exclusions<-NULL
  
  
  #participants<-participants-length(exclusions)
  paramExcl<-param[!param$PartNum %in% exclusions,]
  
  alphas<-paramExcl$alpha
  betas<-paramExcl$beta

  # model name
  
  mod<- ifelse(setup=="exp1", substr(files1[f], 53, nchar(files1[f])-4), 
               substr(files1[f], 52, nchar(files1[f])-4))
  #mod<-"fLR_Instr"
  
  simulation_function<<-get(paste("simulate_", mod, sep=""))
      
  print(paste("working on model", mod))
  
  simAll<-vector()
  
  # create simulated data
  setwd(cd)
  setwd("computational_model")
  for (j in 1:participants){
    
    sim<-simulation_function(T = trials, mu = mu, alpha = alphas[j],
                             beta  = betas[j], initialQ = initialQ )
    
    # convert scene in scene_cat
    names(sim)[1:2]<-c("scene_cat", "response")
    
    # calculate accuracy
    sim$acc<-NA
    for (n in 1:nrow(sim)){
      
      if (sim$response[n]==sim$object_cat[n]){
        
        sim$acc[n]<-1}else{sim$acc[n]<-0}
      
    }
    
    # get cumulative accuracy by scene
    sim<-cumAccbyScene(sim, "acc")
    
    sim$participant<-rep(param$PartNum[j], nrow(sim))
    
    sim$alpha<-alphas[j]
    
    sim$beta<-betas[j]
    
    simAll<- rbind(simAll, sim)
    
  }
  
  setwd(cd)
  
  files_1<-selPhase(1, setup)
  setwd(cd)

  files_2<-selPhase(2, setup)
  setwd(cd)
  # 
  # # merge the two
  # files<-rbind(files_1, files_2)
  # 
  # exclusionfiles<-NA
  # 
  # # exclude participants
  # counter<-1
  # for (h in 1:length(files)){
  #   curfile<-files[h]
  #   # substring the name
  #   subfile<-substr(curfile, 5,6)
  #   # check if it is equal to any number
  #   if (any(exclusions==subfile)){
  #     exclusionfiles[counter] <-files[h]
  #     counter<-counter+1
  #   }
  # }
  
  partAll<-vector()
  # list participants
  
  for (j in 1:participants){
    tryCatch({
    if (setup =="exp1"){
    
    # get the file of the participant for both the phase1 and phase2
    currfile1<-read.csv(paste("exp1/trial_sequences/", 
                              files_1[j], sep=""))
    currfile2<-read.csv(paste("exp1/trial_sequences/", 
                              files_2[j], sep=""))
    } else {
      
      j<-ifelse(j<10, paste0("0",j), j)
      
      currfile1<- 
        read.csv(paste0("exp2/data/BIDS/sub-0",j,
                        "/sub-0",j,"_task-learning_cleaned.csv"))
      
      currfile2<- 
        read.csv(paste0("exp2/data/BIDS/sub-0",j,
                        "/sub-0",j,"_task-enc_cleaned.csv"))
      
      # fix the names
      names(currfile1)[3]<-"trialN"
      
      # delete unnecessary variables
      currfile1<-subset(currfile1,select=-iteration_index)
      currfile2<-subset(currfile2,select=-fillers)
      
      names(currfile1)[which(names(currfile1)=="scn_cat")]<-"scene_cat"
      
      names(currfile2)[which(names(currfile2)=="scn_cat")]<-"scene_cat"
      
    }
    
    # bind them
    currfile<-rbind(currfile1, currfile2)
    # select only scenes 4, 5, or six
    #currfile<-currfile[currfile$scene_cat==4  | currfile$scene_cat==5 
    #                   | currfile$scene_cat==6 ,]
    # order it
    currfile<-currfile[order(currfile$scene_cat),]
    
    # get the trial accuracy variable
    trialAccVar<-ifelse(setup=="exp1", "acc", "trial_acc")
    
    # get cumulative accuracy by scene
    currfile<-cumAccbyScene(currfile, trialAccVar)
    
    # add participant number
    if (setup =="exp1"){
    if (as.numeric(substr(files_1[j], 5,6))<10){
      currfile$participant<-rep(as.numeric(substr(files_1[j], 6,6)),
                                nrow(currfile))
    }else{
      currfile$participant<-rep(as.numeric(substr(files_1[j], 5,6)),
                                nrow(currfile))
    }
    }

    
    currfile$alpha<-param$alpha[param$PartNum==currfile$participant[1]]
    currfile$beta<-param$beta[param$PartNum==currfile$participant[1]]
    
    # append to the dataframe
    partAll<-rbind(partAll, currfile)
    
    },   error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
  }
  
  # we need to bind the two datasets (simulate vs actual)
  # order the partAll
  if (setup=="exp1"){
  partAll<-partAll[, c(1,3,2,4,6,7, 8, 9, 10 )]

  simAll<-simAll[simAll$scene_cat== 1 | simAll$scene_cat== 2  | simAll$scene_cat== 3,]
  
  partAll<-partAll[partAll$scene_cat== 4 | partAll$scene_cat== 5  | partAll$scene_cat== 6,]
  } else {

    simAll$scn_condition<-ifelse(simAll$scene_cat<3, 3, ifelse(simAll$scene_cat==3 |
                                                                 simAll$scene_cat==4, 2,1))
    
    partAll<-partAll[, c("scene_cat", "key_resp_trial.keys", "obj_cat_num", 
                         "trial_acc","trialNbyscene","cumAccbyScene", 
                         "participant", "alpha", "beta", "scn_condition")]
    
    names(partAll)<-names(simAll)
    
    simAll<-simAll[simAll$scn_condition>1 ,]
    
    partAll<-partAll[partAll$scn_condition>1,]

    # from simAll, cut the condition where scn+cond==2 and Ntrialbyscene is >190
    
    simAll<-simAll[!(simAll$scn_condition==2 & simAll$trialNbyscene>195),]
    
  }
  
  
  simAll$type<-rep("Simulated", nrow(simAll))
  partAll$type<-rep("Empirical", nrow(partAll))
  
  # bind the two
  dataAll<-rbind(simAll, partAll)
  
  str(dataAll)
  
  # create early and late trials
  if (setup=="exp1"){

  dfAll<-dataAll %>%
          dplyr::mutate(earlyVsLate=ifelse(trialNbyscene<21, "Early", 
                                    if_else(trialNbyscene>79, "Late","middle")))
  
  } else{
    # lower and upperboundary for early, late, middle
    #early<-max(unique(dataAll$trialNbyscene))/4

    dfAll<-dataAll %>%
      dplyr::mutate(earlyVsLate=ifelse(trialNbyscene<42, "Early", 
              ifelse(trialNbyscene>80, "Late","middle")))
  }
  
  
  # the following analysis of the learning rate 
  # is not possible for the dLR_instrumental model, since it does not 
  # estimate an alpha per participant
  if (files1[f]!= "ParameterEstimation.exp1.betalimit=10.initialQ=0.33.dLR_Instr.csv"   & 
      files1[f]!= "ParameterEstimation.exp2.betalimit=10.initialQ=0.5.dLR_Instr.csv"
      ){
  # select oly strong prior condition
  #dfAll<-dfAll[dfAll$scn_condition>2,]
  # now plot
  # summarise it first
  dfSummarised<- dfAll %>%
    group_by(  alpha, type, earlyVsLate) %>%
    #filter(alpha<0.4) %>% # delete outlier
    dplyr::summarise(mean = mean(acc), sd = sd(acc))
  
  
  # create bins for alpha
  med<-quantile(dfSummarised$alpha)
  
  dfSummarised$bin<-NA
  for (n in 1:nrow(dfSummarised)){
    if (dfSummarised$alpha[n]<med[2]){
      dfSummarised$bin[n]<-1
      
    } else if (dfSummarised$alpha[n]>=med[2] & dfSummarised$alpha[n]<med[3]){
      dfSummarised$bin[n]<-2
      
    } else if (dfSummarised$alpha[n]>=med[3] & dfSummarised$alpha[n]<med[4]){
      dfSummarised$bin[n]<-3
      
    } else {
      dfSummarised$bin[n]<-4
      
    }
  }
  
  # get within participant SE
  # create id
  # dfSummarised$ID<-rep(1:64, each = 3)
  # library(Rmisc)
  # dfSE <- summarySEwithin(dfSummarised,
  #                          measurevar = "mean",
  #                          withinvars = c("earlyVsLate"), 
  #                          betweenvars = c("type", "bin"),
  #                          idvar = "ID")
  # # detach the package
  # detach("package:Rmisc", unload=TRUE)
  # 
  dfSummarised$bin<-as.factor( dfSummarised$bin)
  

  ggplot(dfSummarised[dfSummarised$earlyVsLate!="middle",], aes(x = bin , y =mean)) +
    geom_bar(aes(bin, mean, fill = bin),
             position="dodge",stat="summary", fun.y="mean", SE=T)+
    #geom_errorbar(aes(y = mean, ymin = mean - se, ymax = mean + se),
     #             color = "black", width = 0.10, data=dfSE[dfSE$earlyVsLate!="middle",],)+
    #stat_summary(fun="mean",geom="line")+
    stat_summary(fun.data = "mean_se", size = 0.8, geom="errorbar", width=0.2 )+
    
    #geom_line(stat="summary")+
    theme_bw()+
    facet_grid(earlyVsLate~type)+
    scale_fill_viridis_d()+
    ylab("Cumulative Accuracy")
  
  ggsave(paste0("computational_model/figures/bin.plot.",setup,".jpg"))
  
  expname<-ifelse(setup == "exp1", "Experiment 1", "Experiment 2")
  # all together, without distinction early vs late
  ggplot(dfSummarised[dfSummarised$earlyVsLate!="middle",], aes(x = bin , y =mean)) +
    geom_bar(aes(bin, mean, fill = bin),
             position="dodge",stat="summary", fun.y="mean", SE=T)+
    #geom_errorbar(aes(y = mean, ymin = mean - se, ymax = mean + se),
    #             color = "black", width = 0.10, data=dfSE[dfSE$earlyVsLate!="middle",],)+
    #stat_summary(fun="mean",geom="line")+
    stat_summary(fun.data = "mean_se", size = 0.8, geom="errorbar", width=0.2 )+
    
    #geom_line(stat="summary")+
    theme_classic()+
    facet_grid(.~type)+
    scale_fill_viridis_d()+
    theme(
      plot.title = element_text(size = 22),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text=element_text(size=20)
    )+
    theme(strip.text.x = element_text(size = 22))+
    ggtitle(expname)+
    theme(plot.title = element_text(hjust = 0.5))+
    ylab("Cumulative Accuracy")
  
  ggsave(paste0("computational_model/figures/bin.plot.all.",setup,".jpg"))
  
  
  # analyse
  modBinsEarly<-lm(mean~bin, data = dfSummarised[dfSummarised$earlyVsLate=="Early",])
  summary(modBinsEarly)
  
  modBinsLate<-lm(mean~bin, data = dfSummarised[dfSummarised$earlyVsLate=="Late",])
  summary(modBinsLate)
  
  # plot learning rate actual
  ggplot(param, aes(x=alpha))+
           theme_bw()+
           geom_density()
           
  ggplot(param, aes(x=beta))+
    theme_bw()+
    geom_density()        
  }
  
  expname<-ifelse(setup == "exp1", "Experiment 1", "Experiment 2")
  
  
  if (setup =="exp1"){
  # get the standard error and cumulative accuracy for simALL
  Datawidesim<- simAll %>%
    group_by( trialNbyscene) %>%
    dplyr::summarise(mean = mean(cumAccbyScene), sd = sd(cumAccbyScene)) %>%
    mutate(type ="Simulated" )
  
  # get se
  Datawidesim$se<-Datawidesim$sd/sqrt(participants)
  
  # now for actual data
  Datawidepart<- partAll %>%
    group_by( trialNbyscene) %>%
    dplyr::summarise(mean = mean(cumAccbyScene), sd = sd(cumAccbyScene)) %>%
    mutate(type ="Actual" )
  
  # get se
  Datawidepart$se<-Datawidepart$sd/sqrt(participants)
  
  # bind them
  Datawide<-rbind(Datawidesim, Datawidepart)
  
  # get se
  Datawidepart$se<-Datawidepart$sd/sqrt(participants)
  # now collapsed across participants and scenes together
  Datawide$Type<-Datawide$type
  print(
    ggplot(Datawide, aes(x = trialNbyscene, y=mean, color = Type, fill=Type))+   
      stat_summary(fun.y="mean",geom="line")+ylim(c(0,1))+
      geom_ribbon(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), alpha=0.4, colour=NA)+
      #geom_ribbon(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), alpha=0.2)+
      theme_classic()+
      ylab("Cumulative Accuracy")+
      xlab("Trial Number by Condition")+
      
      theme(
        plot.title = element_text(size = 22),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text=element_text(size=20)
      )+
      theme(strip.text.x = element_text(size = 22))+
      theme(legend.text=element_text(size=22))+
      
      ggtitle(expname)+
      theme(plot.title = element_text(hjust = 0.5))+
      
      scale_color_viridis(discrete=TRUE, option = "viridis")
    

  )
  } else{

  Datawidesim<- simAll %>%
    group_by( trialNbyscene, scn_condition) %>%
    dplyr::summarise(mean = mean(cumAccbyScene), sd = sd(cumAccbyScene)) %>%
    mutate(type ="Simulated" )
  
  # get se
  Datawidesim$se<-Datawidesim$sd/sqrt(participants)
  
  # now for actual data
  Datawidepart<- partAll %>%
    group_by( trialNbyscene,scn_condition) %>%
    dplyr::summarise(mean = mean(cumAccbyScene), sd = sd(cumAccbyScene)) %>%
    mutate(type ="Actual" )
  
  # get se
  Datawidepart$se<-Datawidepart$sd/sqrt(participants)
  
  # bind them
  Datawide<-rbind(Datawidesim, Datawidepart)
  
  Datawide$scn_condition<-as.factor(Datawide$scn_condition)
  levels(Datawide$scn_condition)<-c("0.70","0.90")
  Datawide$Type<-Datawide$type
  # now collapsed across participants and scenes together
  print(
    ggplot(Datawide, aes(x = trialNbyscene, y=mean, color = Type, fill=Type))+   
      stat_summary(fun.y="mean",geom="line")+ylim(c(0,1))+
      geom_ribbon(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), alpha=0.4,colour=NA)+
      theme_classic()+
      facet_wrap(scn_condition~.)+
      ylab("Cumulative Accuracy")+
      xlab("Trial Number by Condition")+
      theme(
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 20),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text=element_text(size=20)
      )+
      theme(strip.text.x = element_text(size = 22))+
     theme(legend.text=element_text(size=22))+

      labs(title = expname,
           subtitle = "Contingency condition")+
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))+
      scale_color_viridis(discrete=TRUE, option = "inferno")
    
    
  )
}
  
 
  
  ggsave(paste("computational_model/figures/SimulatedVsActual.exp=",setup,
               ".mod=", mod, ".png", sep="" ),
         width = 7, height = 7)
  

  # suspend for 20 seconds before printing,
  # to wait that all the graphs are printed
  dev.off()
  
}
}

