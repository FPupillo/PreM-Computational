#------------------------------------------------------------------------------#
# Interaction between BIC and PE
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


# select the first three rows
#exp1lr<-exp1lr[,1:3]
# change the name of the variable indicating the participant
names(exp1lr)[1]<-"SubNum"

exp1All<-merge(exp1, exp1lr, by = "SubNum")

# analyse
PEbyBIC1<-glmer(id_acc~PE*trial_acc*BIC+(trial_acc*PE|SubNum), family= binomial, 
               data = exp1All,
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(PEbyBIC1)
Anova(PEbyBIC1)

# create median split
medianBIC1<-median(exp1lr$BIC)
hist(exp1lr$BIC)

# create median split
#exp1All$high_low_BIC<-ifelse(exp1All$BIC<medianBIC1, "lowBIC", "HighBIC")



# plot 
print(
  ggplot(exp1All, aes( x=PE, y=id_acc))+
    geom_line(stat="smooth",method = "glm",  formula=y~x,method.args=list(family="binomial"), alpha=0.5)+
    aes(colour = factor(SubNum), alpha=0.3)+
    geom_smooth(method="glm",formula=y~x,method.args=list(family="binomial"), colour="black", se=T)+
    theme(strip.text.x = element_text(size = 13))+ ylim(c(0,1))+
    facet_grid(.~high_low_BIC)+
    theme_classic()+
    ggtitle("Experiment 1")+
    #annotate(geom="text",  label="a",size=8,family="serif")+
    theme(legend.position = "none")+
    ylab("p(Hit)")
)

#------------------Experiment2-------------------------------------------------#
exp2<-expAll[expAll$exp=="Experiment2",]

# select the first three rows
exp2lr<-exp2lr
# change the name of the variable indicating the participant
names(exp2lr)[1]<-"SubNum"

# add 200 to lr to match the numbers of exp2
exp2lr$SubNum<-exp2lr$SubNum+200

#check if the numbers are ok
unique(exp2$SubNum)
unique(exp2lr$SubNum)

exp2All<-merge(exp2, exp2lr, by = "SubNum")

# analyse
PEbyBIC2<-glmer(id_acc~PE*trial_acc*BIC+(trial_acc*PE|SubNum), family= binomial, 
               data = exp2All,
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(PEbyBIC2)
Anova(PEbyBIC2)

#------------------Merge-------------------------------------------------#

expAllLr<-rbind(exp1All, exp2All)

# analyse
PEbyLrmerge<-glmer(id_acc~PE*trial_acc*BIC+(trial_acc*PE|SubNum), family= binomial, 
                   data = expAllLr,
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(PEbyLrmerge)
Anova(PEbyLrmerge)




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
# change ref
expAllLr$trial_acc<-as.factor(expAllLr$trial_acc)
expAllLr$trial_acc<-relevel(expAllLr$trial_acc, ref = "Incorrect")

PEnoOut<-glmer(id_acc~PE*trial_acc+(trial_acc*PE|SubNum), family= binomial, 
               data = expAllLr,
               contrasts=list(trial_acc=c(0.5,-0.5)),
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(PEnoOut)

Anova(PEnoOut)
# extract coefficients
coeff<-ranef(PEnoOut)$SubNum$`trial_accCorrect:PE`

# run a correlation 
BICs<- expAllLr$BIC[!duplicated(expAllLr$SubNum)]
plot( BICs~coeff)
abline(lm(BICs ~ coeff))

#check 
a<-lm(BICs ~ coeff)

summary(a)

cor.test(coeff, BICs)

# ggplot
# create a df
df<-as.data.frame(cbind(coeff,BICs ))

ggplot(df, aes(x = coeff,y= BICs ))+
  geom_point()+
  theme_classic()+
  geom_smooth(method=lm, colour = "red")+
  xlab("PExPO coefficients")+
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text=element_text(size=20)
  )+
  scale_x_continuous(breaks=c(-0.50,-0.25,0.00,  0.25))+
  ylab("BIC")

# save
ggsave("figures/PExPO_BIC.jpg", 
       width = 7, height = 7)

