# trying to estimate with sglmertosta
rm(list=ls())
library(rstan)
library(lme4)
cd<-getwd()
 library(devtools)
options(timeout= 4000000)

devtools::install_github('rmcelreath/glmer2stan')
# usethis::use_description()
 library(glmer2stan)

# source the functions for glmer to stan

setwd("premup-pilot/helper_functions/glmer2stanpack")
for (f in 1:length(list.files())){
  source(list.files()[f])
}

setwd(cd)

##### glmer2stan ####
# - allows user to write mixed effects formulas using lme4 syntax
# - converts formula and data to STAN friendly formats
# - returns stanmer object
# - computes WAIC (a bayesian model comparison statistic) (Widely applicable information criterion)

# load data
Datalong<-read.csv2("premup-pilot/outputs/group_level/share/group_task-rec.csv",sep=";",
                    header=T,stringsAsFactors=FALSE)
# convert PE to factor
Datalong$PE_level<-as.factor(Datalong$PE_level)

# select only old trials
Datalong<-Datalong[(Datalong$OvsN==1),]

# exclude fillers
Datalong<-Datalong[Datalong$fillers==0,]


# no random effect model. It is not taking into account that there are several participants. 
Datalong$id_acc<-as.numeric(Datalong$id_acc)
m1_lm<- glm(id_acc~PE_level,family = binomial, data=Datalong)

summary(m1_lm)

# now random
m1_lme4<- glmer(id_acc~PE_level + (PE_level|participant),family = binomial, data=Datalong)

summary(m1_lme4)

# compare the two models using AIC
AIC(m1_lm, m1_lme4)
BIC(m1_lm, m1_lme4)

# Stan does not deal with non-numeric factors. We need to convert to integers
# even participants identifyier

Datalong$PE_levelNum<-as.integer(as.factor(Datalong$PE_level))

# basic MCMC parameters
nwarm = 100 # burn in period, samples that are not included in the estimation
niter = 500 # number of steps per chain
chains = 4# number of chains, at least 2

m1_g2s<-glmer2stan(id_acc~PE_level + (PE_level|participant),
                   data=Datalong, 
                   calcWAIC = T,
                   warmup = nwarm, 
                   iter = niter,
                   chains = chains)

                                 