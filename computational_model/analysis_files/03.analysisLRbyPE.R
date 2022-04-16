#------------------------------------------------------------------------------#
# Interaction between learning rate and PE
# Cerated: ""Wed Feb  2 18:18:10 2022"
#------------------------------------------------------------------------------#

rm(list=ls())
# load the packages
library(data.table)
library(dplyr)
library(viridis)
library(lme4)
library(car)

setwd("computational_model")
# load the data of the two experiments
# recognition
#exp1rec<-fread("exp1/outputs/group_level/share/group_task-rec.csv")

expAll<-fread("output_files/fittedData_exp1-exp2_fLR_Instr.csv")

# learning rate
exp1lr<-fread(paste0("/home/francesco/PowerFolders/Frankfurt_University/PIVOTAL/PREMUP_computational/exp1/outputs/group_level/computational_model/", 
                     "ParameterEstimation.exp1.betalimit=10.initialQ=0.33.fLR_Instr.csv"))

exp2lr<-fread(paste0("/home/francesco/PowerFolders/Frankfurt_University/PIVOTAL/PREMUP_computational/exp2/outputs/group_level/computational_model/", 
                     "ParameterEstimation.exp2.betalimit=10.initialQ=0.5.fLR_Instr.csv"))
#------------------Experiment1-------------------------------------------------#
exp1<-expAll[expAll$exp=="Experiment1",]
# change ref
exp1$trial_acc<-as.factor(exp1$trial_acc)
exp1$trial_acc<-relevel(exp1$trial_acc, ref = "Incorrect")

# select the first three rows
exp1lr<-exp1lr[,1:3]
# change the name of the variable indicating the participant
names(exp1lr)[1]<-"SubNum"

exp1All<-merge(exp1, exp1lr, by = "SubNum")

# analyse
PEbyLr1<-glmer(id_acc~PE*trial_acc*alpha+(trial_acc*PE|SubNum), family= binomial, 
                  data = exp1All,
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(PEbyLr1)
Anova(PEbyLr1)

# create median split
medianAlpha1<-median(exp1lr$alpha)
hist(exp1lr$alpha)

# create median split
exp1All$high_low_LR<-ifelse(exp1All$alpha<medianAlpha1, "lowLR", "HighLR")



# plot 
print(
  ggplot(exp1All, aes( x=PE, y=id_acc))+
    geom_line(stat="smooth",method = "glm",  formula=y~x,method.args=list(family="binomial"), alpha=0.5)+
    aes(colour = factor(SubNum), alpha=0.3)+
    geom_smooth(method="glm",formula=y~x,method.args=list(family="binomial"), colour="black", se=T)+
    theme(strip.text.x = element_text(size = 13))+ ylim(c(0,1))+
    facet_grid(.~high_low_LR)+
    theme_bw()+
    ggtitle("Experiment 1")+
    #annotate(geom="text",  label="a",size=8,family="serif")+
    theme(legend.position = "none")+
    ylab("p(Hit)")
)

#------------------Experiment2-------------------------------------------------#
exp2<-expAll[expAll$exp=="Experiment2",]
# change ref
exp2$trial_acc<-as.factor(exp2$trial_acc)
exp2$trial_acc<-relevel(exp2$trial_acc, ref = "Incorrect")

# select the first three rows
exp2lr<-exp2lr[,1:3]
# change the name of the variable indicating the participant
names(exp2lr)[1]<-"SubNum"

# add 200 to lr to match the numbers of exp2
exp2lr$SubNum<-exp2lr$SubNum+200

#check if the numbers are ok
unique(exp2$SubNum)
unique(exp2lr$SubNum)

exp2All<-merge(exp2, exp2lr, by = "SubNum")

# analyse
PEbyLr2<-glmer(id_acc~PE*trial_acc*alpha+(trial_acc*PE|SubNum), family= binomial, 
               data = exp2All,
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(PEbyLr2)
Anova(PEbyLr2)

#------------------Merge-------------------------------------------------#

expAllLr<-rbind(exp1All[, 1:12], exp2All)

# analyse
PEbyLrmerge<-glmer(id_acc~PE*trial_acc*alpha+(trial_acc*PE|SubNum), family= binomial, 
               data = expAllLr,
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(PEbyLrmerge)
Anova(PEbyLrmerge)

# delete outlier
expAllLr_noOut<-expAllLr[expAllLr$alpha<0.5, ]
hist(expAllLr_noOut$alpha)

# refit
PEbyLr_noOut<-glmer(id_acc~PE*trial_acc*alpha+(trial_acc*PE|SubNum), family= binomial, 
               data = expAllLr_noOut,
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(PEbyLr_noOut)
Anova(PEbyLr_noOut)

# create median split

medianAlpha<-median(expAllLr_noOut$alpha)

expAllLr_noOut$high_low_LR<-ifelse(expAllLr_noOut$alpha<=medianAlpha, "lowLR", "HighLR")

Anova(expAllLr_noOut)


# plot 
print(
  ggplot(exp1All, aes( x=PE, y=id_acc))+
    geom_line(stat="smooth",method = "glm",  formula=y~x,method.args=list(family="binomial"), alpha=0.5)+
    aes(colour = factor(SubNum), alpha=0.3)+
    geom_smooth(method="glm",formula=y~x,method.args=list(family="binomial"), colour="black", se=T)+
    theme(strip.text.x = element_text(size = 13))+ ylim(c(0,1))+
    facet_grid(.~high_low_LR)+
    theme_bw()+
    ggtitle("Experiment 1")+
    #annotate(geom="text",  label="a",size=8,family="serif")+
    theme(legend.position = "none")+
    ylab("p(Hit)")
)

#-----------------extract the coefficient--------------------------------------#

#### Exp1
coeff1<-ranef(PEbyLr1)$SubNum$`trial_accCorrect:PE`

# run a correlation 
LRs1<- exp1All$alpha[!duplicated(exp1All$SubNum)]

plot( LRs1~coeff1)
abline(lm(LRs1 ~ coeff1))

# delete the outlier and try again
exp1AllnoOut<-exp1All[exp1All$alpha<0.5,]

# refit the model
PEbyLr1_noOut<-glmer(id_acc~PE*trial_acc*alpha+(trial_acc*PE|SubNum), family= binomial, 
               data = exp1AllnoOut,
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

coeff1_noOut<-ranef(PEbyLr1_noOut)$SubNum$`trial_accCorrect:PE`

# run a correlation 
LRs1noOut<- exp1AllnoOut$alpha[!duplicated(exp1AllnoOut$SubNum)]

plot( LRs1noOut~coeff1_noOut)
abline(lm(LRs1noOut ~ coeff1_noOut))

mod1<-lm(LRs1noOut ~ coeff1_noOut)
summary(mod1)

#### Exp2
coeff2<-ranef(PEbyLr2)$SubNum$`trial_accCorrect:PE`

# run a correlation 
LRs2<- exp2All$alpha[!duplicated(exp2All$SubNum)]

plot( LRs2~coeff2)
abline(lm(LRs2 ~ coeff2))
mod2<-lm(LRs2 ~ coeff2)

summary(mod2)
#### Merged


PEnoOut<-glmer(id_acc~PE*trial_acc+(trial_acc*PE|SubNum), family= binomial, 
                    data = expAllLr_noOut,contrasts=list(trial_acc=c(-0.5,0.5)),
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(PEnoOut)


# extract coefficients
coeff<-ranef(PEnoOut)$SubNum$`trial_accCorrect:PE`

# run a correlation 
LRs<- expAllLr_noOut$alpha[!duplicated(expAllLr_noOut$SubNum)]
plot( LRs~coeff)
abline(lm(LRs ~ coeff))

#check 
Mod_merged<-lm(LRs ~ coeff)

summary(Mod_merged)

corr<-cor.test(LRs, coeff)

corr
