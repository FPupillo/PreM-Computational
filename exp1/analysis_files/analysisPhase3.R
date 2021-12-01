# Behavioural analysis script
rm(list=ls())

library(ggplot2)
library(dplyr)
library(psycho)
library(lme4)
library(sjstats)
library(brms)


cd<-getwd()
setwd("premup-pilot")
source("helper_functions/discCalc.R")

# in this dataset, there are 313 old items. by 30 participants, there are 9360 trials. 

Datalong<-read.csv2("outputs/group_level/share/group_task-rec.csv",sep=";",
                     header=T,stringsAsFactors=FALSE)

# calculate discriminability by participant
# first, create wide dataset
Datawide<-discCalc(Datalong)

# histogram dprim
hist(Datawide$dprime)

# participants who got less than 1+2SD
outcut<-mean(Datawide$dprime)

excl<-Datawide$SubNum[Datawide$dprime<outcut-2* sd(Datawide$dprime)]

# select only old trials
Datalong<-Datalong[(Datalong$OvsN==1),]

# exclude fillers
Datalong<-Datalong[Datalong$fillers==0,]

# count by trials
countTrials<-Datalong %>%
  group_by(PE_level) %>%
  tally()

# divide by participants
countTrials$n<-countTrials$n/length(unique(Datalong$participant))

countTrials

# exclude participant
Datalong<-Datalong[!Datalong$participant %in% excl, ]
      
# aggregate data by session, condition, participants
data_agg<-Datalong %>%
  group_by(session, PE_level, participant) %>%
  dplyr::summarise(id_acc = mean(id_acc, na.rm = T))

# congert the factors variables
data_agg$PE_level<-as.factor(data_agg$PE_level)
data_agg$session<-as.factor(data_agg$session)

# rename the levels
levels(data_agg$PE_level)<-c("LowPE", "MediumPE", "HighPE")
levels(data_agg$session)<-c("ImmediateRec", "DelayedRec")

# get within participant SE
library(Rmisc)
dat_summary <- summarySEwithin(data_agg,
                                measurevar = "id_acc",
                                withinvars = c("PE_level", "session"), 
                                idvar = "participant")




ggplot(data_agg, aes(PE_level, id_acc))+ 
  geom_bar(aes(PE_level, id_acc, fill = PE_level),
       position="dodge",stat="summary", fun.y="mean")+
 
  geom_errorbar(aes(y = id_acc, ymin = id_acc - ci, ymax = id_acc + ci),
              color = "black", width = 0.10, data=dat_summary)+
  #geom_boxplot(alpha =0.1)+
  geom_jitter( size=0.4,width=0.1, data=data_agg)+
  theme_bw()+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  facet_grid(.~session)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylim(c(0,1))+
  ylab("% recognition")

# unconditional model first to check the intraclass correlation
unconditionalM<-glmer(id_acc~(1|participant), family=binomial(), data= Datalong)
summary(unconditionalM)

# intraclass correlation
ic1<-performance::icc(unconditionalM)
# 15% of the variance in recognition memory is between participants

# does adding random intercepts for images improve the fit?
unconditionalMandIM<-glmer(id_acc~(1|participant)+(1|rec_Obj), family=binomial(), data= Datalong)
summary(unconditionalMandIM)

anova(unconditionalM, unconditionalMandIM)
# significant improvement

# does adding random slopes for PE improves it?
# first, convert PE level into factor
Datalong$PE_level<-as.factor(Datalong$PE_level)
unconditionalMimandSesPE<-glmer(id_acc~(1+PE_level|participant)+(1|rec_Obj), family=binomial(), data= Datalong,
                                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# singular fit

anova(unconditionalMandIM, unconditionalMimandSesPE)
# does not improve the fit



# now add the fixed effect and the maximal random structure
modelmaximal<-glmer(id_acc~PE_level*session+(1+session|participant)+(1|rec_Obj), 
                    family=binomial(), data= Datalong,
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(modelmaximal)

Anova(modelmaxima, type = )
# frop the random slope for PE
modeldropPE<-glmer(id_acc~PE_level*session+(session|participant)+(1|rec_Obj), 
                    family=binomial(), data= Datalong,
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(modeldropPE)

Anova(modeldropPE)

# set controasts
Datalong$PE_level<-as.factor(Datalong$PE_level)
Datalong$session<-as.factor(Datalong$session)
# test for a quadratic trend: u-saped/ inverted u shaped
contrast1<-c(1,-2,1)
# linear
contrast2<-c(-1,0,1)

contrasts(Datalong$PE_level)<-cbind(contrast1, contrast2)

modeldropPE<-glmer(id_acc~PE_level*session+(session|participant)+(1|rec_Obj), 
                   family=binomial(), data= Datalong,
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(modeldropPE)

# let's check contr.poly function, with three levels
contrasts(Datalong$PE_level)<-contr.poly(3)

modeldropPE<-glmer(id_acc~PE_level*session+(session|participant)+(1|rec_Obj), 
                   family=binomial(), data= Datalong,
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(modeldropPE)

# similar

# bayesian
library(MCMCglmm)

# create a dataset without missing data
Datalong_nomiss<-Datalong[complete.cases(Datalong$id_acc),]
# it is the same, so keep using the previous one. 

contrasts(Datalong_nomiss$PE_level)<-contr.poly(3)


Datalong$id_acc<-as.factor(Datalong$id_acc)

bayesianinteract<-MCMCglmm(id_acc~PE_level,
                             random = ~idh(PE_level):participant + rec_Obj,
                           #family = "categorical",
                           data=Datalong_nomiss)
                           #nitt=3000, thin=10, burnin=500)
 
summary(bayesianinteract)

Anova
plot(bayesianinteract)

bayesianinteract<-MCMCglmm(id_acc~PE_level*session,
                           random = ~idh(session*PE_level):participant + rec_Obj, 
                           #family = "ordinal",
                           data=Datalong,
                           nitt=100000, thin=50, burnin=3000)

plot(bayesianinteract)

summary(bayesianinteract)

# nrms  
options(timeout= 4000000)

fit_b<-brm(id_acc~session+(PE_level|participant)+(1|rec_Obj),   #similar to lmwr
           family = bernoulli(link = "logit"),
           warmup = 500, 
           iter = 2000, 
           chains = 2, 
           inits = "0", 
cores=2,
data=Datalong)

plot(fit_b)
summary(fit_b)
        
# specify priors
prior1 <- c(
  prior(normal(0, 10), class = Intercept), # prior distribution for the intercept
  prior(normal(0, 10), class = b, coef = session), # prior distribution for the effect of session
  prior(cauchy(0, 10), class = sigma) # standard deviation of the residuals 
)

fit_b<-brm(id_acc~session,   #similar to lmwr
           family = gaussian(),
           warmup = 500, 
           iter = 2000, 
           chains = 2, 
           prior = prior1,
           inits = "0", 
           cores=2,
           data=Datalong)

get_prior(id_acc~session+(session|participant)+(1|rec_Obj),   #similar to lmwr
          family = bernoulli(link = "logit"),data = Datalong)
  

# add intercept
prior2 <- c(
  prior(normal(0, 10), class = Intercept), # prior distribution for the intercept
  prior(normal(0, 10), class = b, coef = session), # prior distribution for the effect of session
  prior(cauchy(0, 10), class = sd), # standard deviation of the by-subjects intercepts
  prior(cauchy(0, 10), class = sigma) # standard deviation of the residuals 
)

fit_b<-brm(id_acc~session +(1|participant),   #similar to lmwr
           family = gaussian(),
           warmup = 500, 
           iter = 2000, 
           chains = 2, 
           prior = prior3,
           inits = "0", 
           cores=2,
           data=Datalong)

# we can youse the same priors as previously for including random slopes: the same sd of the intercept is used #
# for the random slopes ditr

fit_b<-brm(id_acc~session +(1+session|participant),   #similar to lmwr
           family = gaussian(),
           warmup = 500, 
           iter = 2000, 
           chains = 2, 
           prior = prior2,
           inits = "0", 
           cores=2,
           data=Datalong)

stanplot(fit_b, 
         type = "areas",
         prob = 0.95)

prior2 <- c(
  prior(normal(0, 10), class = Intercept), # prior distribution for the intercept
  prior(normal(0, 10), class = b, coef = session), # weakly informative prior distribution for the effect of session
  prior(normal(0, 10), class = b, coef = PE_level), # weakly informative prior distribution for the effect of session
  prior(normal(0, 10), class = b, coef = PE_level:session), # interaction
  prior(cauchy(0,10), class = sd) # standard deviation of the by-subjects intercepts
  
)

#relevel PE
levels(Datalong$PE_level)<-c(-1, 2,-1)

# turn to numeric
Datalong$PE_level<-as.integer(as.factor(Datalong$PE_level))

fit_b<-brm(id_acc~PE_level*session +(1+PE_level*session|participant),   #similar to lmwr
           family = bernoulli(link="logit"),
           warmup = 500, 
           iter = 2000, 
           chains = 2, 
           prior = prior2,
           inits = "0", 
           cores=1,
           data=Datalong)

stanplot(fit_b, 
         type = "areas",
         prob = 0.95)

plot(fit_b)
