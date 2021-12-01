# function that computes the variance partitioning
varPart<-function(df, var1, var2){
  #---------------------------------------------------------------------------------------------#
  # function that computes the variance partitioning
  # INPUT: df - dataframe
  #        var1 - first variable
  #        var2 - second variable
  # OUTPUT: a vector with a, c,b : variance explaind
  #         by variable 1, 2, and commond variance
  #         respectively
  #---------------------------------------------------------------------------------------------#
DataAll<-df
  
  PEAllMod<-glmer(id_acc~ poly(PEchoice,2)+poly(PEobs,2)+( 1+poly(PEchoice,2)+poly(PEobs,2)|  SubNum), family = binomial,
                  data = DataAll[!is.na(DataAll$PEchoice)&
                                   !is.na(DataAll$PEobs) &
                                   DataAll$session=="ImmediateRec",],
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
  
  PErespMod<-glmer(id_acc~ poly(PEchoice,2)+( 1+poly(PEchoice,2)| SubNum), family = binomial,
                   data = DataAll[!is.na(DataAll$PEchoice)&
                                    !is.na(DataAll$PEobs) &
                                    DataAll$session=="ImmediateRec",],
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
  
  
  PEobsMod<-glmer(id_acc~ poly(PEobs,2)+( 1+poly(PEobs,2)| SubNum), family = binomial,
                  data = DataAll[!is.na(DataAll$PEchoice)&
                                   !is.na(DataAll$PEobs)&
                                   DataAll$session=="ImmediateRec",],
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
  
  # [a+b+c]
  RsquaredAll<-r2_nakagawa(PEAllMod)[[2]]
  
  # [a+b]
  RsquaredPEresp<-r2_nakagawa(PErespMod)[[2]]
  
  # [b+c]
  RsquaredPEobs<-r2_nakagawa(PEobsMod)[[2]]
  
  # b<- ab+bc-abc
  b<- (RsquaredPEresp+RsquaredPEobs) -RsquaredAll
  
  #a<-ab-b
  a<-RsquaredPEresp-b
  
  #c<-bc-b
  c<-RsquaredPEobs-b
  
  output<-c(a,c,b)
  
  return(output)
  
}