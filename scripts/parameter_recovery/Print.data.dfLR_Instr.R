#------------------------------------------------------------------------------#
# Print graph for parameter RW optimal bayesian
# Arguments: setup - exp1 or exp2
# Cerated: "Mon Nov 15 09:51:18 2021"
#------------------------------------------------------------------------------#

rm(list=ls())

library(ggplot2)
library(gridExtra) # for plotting
library(ggpubr)

betalim<-10

model<-"dfLR_Instr"

Args<-commandArgs(trailingOnly = T)
setup<-Args[1]

setup<-"exp2"

if (setup=="exp1"){
  initialQ<-0.33
} else {
  initialQ<-0.50
}

name<- paste("output_files/parameterRecovery.",setup,".", model, ".", 
             "betalimit=",  betalim,  
             ".initialQ=", initialQ , sep="")

# retrieve the file
parameterRecov<-read.csv(paste0(name, ".csv"))

plotalpha<-ggplot(parameterRecov, aes(x=simAlpha, y=fitAlpha)) + 
  geom_point()+
  geom_smooth(method=lm)+
  theme_bw()+
  stat_cor(method="pearson")+
  xlim(0,1)+
  ylim(0,1)+
  #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
  ggtitle("Alpha parameter")

plotbeta<-ggplot(parameterRecov, aes(x=simBeta, y=fitBeta)) + 
  geom_point()+
  geom_smooth(method=lm)+
  theme_bw()+
  stat_cor(method="pearson")+
  xlim(0,10)+
  ylim(0,10)+
  #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
  ggtitle("Beta parameter")
  
g<-grid.arrange(plotalpha, plotbeta, ncol=2)

arrangeGrob( plotalpha, plotbeta,ncol=1)

# save
ggsave(file=paste0("figures/ParameterRecovery.", setup,".",
                   model, "betalimit=", betalim,  ".jpg"), g)
       