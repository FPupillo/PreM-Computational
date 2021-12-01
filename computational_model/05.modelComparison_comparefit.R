#------------------------------------------------------------------------------#
# Scripts that runs model comparison
# and plots the output
# ""Mon Nov 29 14:32:05 2021"
# argument: setup (exp1 or exp2)
#------------------------------------------------------------------------------#

library(lme4)
library(lmerTest)
library(car)
library(reshape2)
library(ggplot2)
library(dplyr)
library(ggjoy)

rm(list=ls())

# set current directory in the parent folder
#setwd(dirname(getwd()))

cd<-getwd()

# function that takes the arguments from the command line
Args<-commandArgs(trailingOnly = T)

# the only argument is setup
setup<-Args[1]

# for debugging purposes
setup<-c( "exp1")

#setup<-ifelse(setup=="exp1", "pilot", "three")

#for (s in setups){
# s<-setup 
#  print(s)
  
  # retrieve the different models
  setwd(paste0( setup,"/outputs/group_level/computational_model"))
  
  if (setup=="exp1"){ # if this is the pilot, the Q is 0.33
    
    # list the files with parameter estimation
    files<-list.files(pattern="ParameterEstimation.exp1.betalimit=10.initialQ=0.33.*")
    # and, as the name is longer, sartpoint for substringing is earlier
    startStr<-53
    
  }else{
    
  # list the files with parameter estimation
  files<-list.files(pattern="ParameterEstimation.exp2.betalimit=10.initialQ=0.5.*")

  # startpoint is 48
  startStr<-52
  
  }
  
  # create dataset to store the bic
  BicAll<-vector()
  
  # loop through the files
  for (n in 1:length(files)){
    
    # read the first files
    cfile<-read.csv(files[n])
    
    # select parameters
    cfile<-cfile[,c("PartNum", "BIC", "LogLikel")]
    
    # assing the name of the model
    modname<-substr(files[n], startStr, nchar(files[n])-4 )
    
    # assing the identifier
    cfile$model<-rep(modname, times=nrow(cfile))
    
    # assign to the dataframe
    BicAll<-rbind(BicAll, cfile)
    
  }
  
  setwd(cd)
  
  
  # if it is exp 2, we need only the first 40
  if (setup=="exp2"){
    BicAll<-BicAll[BicAll$PartNum<41,]
  }
  
  # subset
  
  #BicAll<-BicAll[, c(1,6,7,8)]
  
  # Count for how many participants a precise model was the best fit
  # create two datasets = one for the LL and one for the BIC
  
  # try to convert to wide
  library(reshape2)
  
  # Model of interest
 MoI<- c("dLR_Instr", "fLR_Instr", "fLR_Eval")
  #BicAll<-BicAll[BicAll$model==MoI[1] | BicAll$model==MoI[2] 
               #  | BicAll$model==MoI[3],]
  
  BicAll_wideBIC <- dcast(BicAll, PartNum ~ model, value.var=c("BIC"), 
                          fun.aggregate =sum)
  BicAll_wideLL <- dcast(BicAll, PartNum ~ model, value.var=c("LogLikel"),
                         fun.aggregate =sum)

  # find the best model for each participant according to BIC
  # (the model that minimize the BIC)

  
  ggplot(BicAll, aes(x = model,y= BIC))+
    geom_bar(aes(model, BIC, fill = model),
             position="dodge",stat="summary", fun="mean", SE=T)+
    stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar",
                 width=0.2 )+
    #geom_jitter( size=1,width=0.1)+
    theme(axis.text.x = element_blank())+ # we are showing the different levels 
    # through the colors, so we can avoid naming the bars
    theme(axis.ticks.x = element_blank())+
    
    theme_bw()
  
  # descriptives
  # get within participant SE
  library(Rmisc)
  table <- summarySEwithin(BicAll,
                                 measurevar = "BIC",
                                 withinvars = c("model"), 
                                 #betweenvars = "session",
                                 idvar = "PartNum")
  # detach the package
  detach("package:Rmisc", unload=TRUE)
  
  # table<-BicAll %>%
  # group_by(model)%>%
  # summarise(Bic = mean(BIC), seBic = sd(BIC),
  # LL=mean(LogLikel), seLL = sd(LogLikel))
  
  # print
  write.csv(table,paste0("computational_model/output_files/TableBIC.LL.", 
                         setup, ".csv"))
  
  # analyse
  BicModel<-lmer(BIC~model+(1|PartNum), data = BicAll)
  
  print(
  summary(BicModel)
  )
  
  anova(BicModel)
  
  BicAll_wideBIC$BestModel<-NA
  BicAll_wideBIC$SecondBest<-NA
  
  for (j in 1: nrow(BicAll_wideBIC)){
    tryCatch({
     # index<-which(BicAll_wideBIC[j,]==min(BicAll_wideBIC[j,(2:(length(files)))], na.rm=T))
      
     
      index<-which(BicAll_wideBIC[j,]==min(BicAll_wideBIC[j,MoI]))
      index2<-which(BicAll_wideBIC[j,]==unlist(sort(BicAll_wideBIC[j,MoI])[2]))
      if (length(index)>1) {# fi there are more than one model
        BicAll_wideBIC$BestModel[j]<-NA
        BicAll_wideBIC$SecondBest[j]<-NA
        
      }else{
      BicAll_wideBIC$BestModel[j]<-names(BicAll_wideBIC[index])
      BicAll_wideBIC$SecondBest[j]<-names(BicAll_wideBIC[index2])
      
      }
    }, error = function(e) { print(paste("problem with number", j))}, 
       warning = function(f) { print(paste("warning with number", j))}) 
  }
  
  # create difference between the best model and the second best
  BicAll_wideBIC$BestModelBIC<-NA
  BicAll_wideBIC$SecondBestModelBIC<-NA
  
  for (n in 1: nrow(BicAll_wideBIC)){
    tryCatch({
 BicAll_wideBIC$BestModelBIC[n]<-BicAll_wideBIC[n,BicAll_wideBIC$BestModel[n]]
 BicAll_wideBIC$SecondBestModelBIC[n]<-BicAll_wideBIC[n,BicAll_wideBIC$SecondBest[n]]
    }, error=function(e){}
    )
  }
  
  # create the difference between the two
  BicAll_wideBIC$BicDiff<-BicAll_wideBIC$SecondBestModelBIC -BicAll_wideBIC$BestModelBIC
  
  # create the evidence
  BicAll_wideBIC$evidence<-NA
  for (n in 1: nrow(BicAll_wideBIC)){
    tryCatch({
      
       if(BicAll_wideBIC$BicDiff[n]>10){
         BicAll_wideBIC$evidence[n]<-"very strong"
       } else if (BicAll_wideBIC$BicDiff[n]<10 & (BicAll_wideBIC$BicDiff[n]>=6)){
         BicAll_wideBIC$evidence[n]<-"strong"
         
       }else if ( BicAll_wideBIC$BicDiff[n]<6 & (BicAll_wideBIC$BicDiff[n]>=2)){
         BicAll_wideBIC$evidence[n]<-"positive"
         
       } else{
         BicAll_wideBIC$evidence[n]<-"weak"
         
       }
    }, error=function(e){}
)
  }
  
  # now long
  BicAll_long<-melt(BicAll_wideBIC[, c(1, 5, 10)], id.vars = c("PartNum", "BestModel" ))
  
  BicAll_long$value<-as.factor(BicAll_long$value)
  levels(BicAll_long$value)<-c("very strong", "strong", "positive", "weak")
  
  names(BicAll_long)[4]<-"Evidence"
  
  # reorder the levels (models)
  BicAll_long$BestModel<-factor(BicAll_long$BestModel, levels =MoI)
  
  ggplot(BicAll_long[!is.na(BicAll_long$Evidence),], 
         aes( x=BestModel, fill = Evidence)) + 
    geom_bar(position="stack", stat="count")+
    #scale_fill_grey()+
    ylab("Participants")+
    coord_flip()+
    theme_bw()+
    scale_fill_viridis_d()
  
  
  ggsave(paste("computational_model/figures/model_comparison_",setup, ".jpg", sep=""))
  
  print(# count best model
  Best<- BicAll_long %>%
         dplyr::count(BestModel)
  )
  
  print(
   strong<- BicAll_long %>%
     group_by(BestModel) %>%
     dplyr:: count(Evidence)
  )



