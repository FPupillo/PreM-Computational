#------------------------------------------------------------------------------#
# Model-free analysis
# Cerated: "Tue Apr 19 18:24:44 2022"
#------------------------------------------------------------------------------#

rm(list=ls())
# load the packages
library(data.table)
library(dplyr)
library(viridis)
library(lme4)
library(car)
library(ggplot2)

# source the scripts
source("computational_model/helper_functions/dprime_thres.R")
source("computational_model/helper_functions/discCalc.R")


# load the data of the two experiments
exp1<-fread("exp1/outputs/group_level/share/group_task-rec.csv")

#------------------Experiment1-------------------------------------------------#
# select only immediate
exp1<-exp1[exp1$session==1,]

# rename fillers for old (as they were NA)
exp1$fillers[exp1$OvsN==2] <- 0

# Remove fillers
exp1 <- exp1[exp1$fillers!=1,]


# are there fillers?
unique(exp1$fillers)

# check how many trials
trials<-exp1 %>%
  #dplyr::filter(fillers == 0) %>%
  group_by( participant,fillers, OvsN, session) %>%
  tally()



#------------------Experiment2-------------------------------------------------#
exp2<-fread("exp2/outputs/group_level/share/group_task-rec.csv")

#The data file contains a lot of information that we will not need now.
exp2 <- subset(x = exp2,
               subset = !is.na(exp2$participant.y),
               select = c("practice", "participant.y", "OvsN",  "id_acc", 
                          "trial_acc", "contingency", "fillers",  "session"))
exp2$participant <- exp2$participant.y

exp2$fillers[exp2$OvsN==2] <- 0



# select only immediate
exp2<-exp2[exp2$participant<41,]
# select only where practice == 0
#exp2<-exp2[exp2$practice==0,]

# delete the fillers
#exp2<-exp2[exp2$fillers==0,]

# check how many trials
trials2<-exp2 %>%
  #dplyr::filter(fillers == 0) %>%
  group_by( participant, OvsN) %>%
  tally()

# 100 old and 100 new
# cr

# now calculate the hit rate by accuracy
#-----------------hitbyAcc------------------------------------------------#
names(exp1)
names(exp2)
# select only old 
exp1<-exp1[exp1$OvsN==1,]
exp2<-exp2[exp2$OvsN==1,]

# create prediction strength
exp1$prediction_condition<-ifelse(exp1$contingency=="0,33", "0.33", "0.2/0.80")
exp1$prediction_condition<-as.factor(exp1$prediction_condition)
#levels(exp1$prediction_condition)<-c("0.2", "0.33", "0.8")
#exp2$prediction_condition<-ifelse(exp2$PE_level=="0,5", "Flat", ifelse(exp2$PE_level=="0,7"
#                                                             | exp2$PE_level == "0,3", "Weak", "Strong" ))

exp2$prediction_condition<- ifelse(exp2$contingency=="0,5", "0.50", 
                                    ifelse(exp2$contingency=="0,1" |exp2$contingency=="0,9", "0.10/0.90",
                                           "0.30/0.70"))



#exp1$prediction_condition<-exp1$contingency
#exp2$prediction_condition<-exp2$PE_level

# we only need encoding accuracy and 
VoI1<-c("participant", "prediction_condition", "enc_acc", "id_acc")

exp1<-exp1[, ..VoI1]

names(exp1)[c(3,4)]<-c("prediction_accuracy", "recognition_accuracy")

VoI2<-c("participant", "prediction_condition",  "trial_acc", "id_acc")

exp2<-exp2[, ..VoI2]

names(exp2)[c(3,4)]<-c("prediction_accuracy", "recognition_accuracy")

# bind them
exp2$participant<-exp2$participant+200

exp1$experiment<-"Experiment1"
exp2$experiment<-"Experiment2"

allData<-rbind(exp1, exp2)

#-----------------Experiment1------------------------------------------------#
# prediction condition
# exclude participants with low performance in phase1
exclPhase1<-c(7 ,16, 19, 20, 23)

exp1<-exp1[!exp1$participant %in% exclPhase1, ]


### aggregate
# take the se within-participant
data_agg_exp1<-exp1 %>%
  group_by(prediction_condition, participant) %>%
  dplyr::summarise(rec_acc = mean(recognition_accuracy, na.rm = T),
                   experiment = first(experiment))
  

data_agg_exp1_acc<-exp1 %>%
  group_by(  prediction_accuracy, participant) %>%
  dplyr::summarise(rec_acc = mean(recognition_accuracy, na.rm = T), 
                   experiment = first(experiment))


library(Rmisc)
dat_summary_exp1 <- summarySEwithin(data_agg_exp1,
                               measurevar = "rec_acc",
                               withinvars = c("prediction_condition"), 
                               idvar = "participant")


data_agg_exp1_acc$prediction_accuracy<-factor(data_agg_exp1_acc$prediction_accuracy)
data_agg_exp1_acc$prediction_accuracy<-relevel(data_agg_exp1_acc$prediction_accuracy, 
                                                  "Incorrect")


gplot_exp1_pred<-ggplot(data_agg_exp1, aes( x=prediction_condition, y=rec_acc))+
  geom_bar(aes(prediction_condition, rec_acc, fill = prediction_condition),
           position="dodge",stat="summary", fun.y="mean", SE=F)+
  geom_jitter(width = 0.20 )+
  
  geom_errorbar(aes(y = rec_acc, ymin = rec_acc - se, ymax = rec_acc + se),
                color = "black", width = 0.10, data=dat_summary_exp1)+
  #facet_wrap(prediction_accuracy~.)+
  theme_classic()+
  ylab("% Hit")+
  theme(legend.position = "none")+
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text=element_text(size=20)
  )+
  xlab("Contingency")+
  ggtitle("Experiment 1")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values =   c("#117733", "#CC6677"))

  #scale_fill_viridis(discrete=TRUE, option = "magma") 

gplot_exp1_pred

# save it
ggsave(paste0("/home/francesco/PowerFolders/Frankfurt_University/PIVOTAL/",
              "PREMUP_computational/computational_model/figures/prediction_cond_exp1.png"), 
       width = 7, height = 7)


# is that significant?
exp1$prediction_condition<-as.factor(exp1$prediction_condition)
modexp1_prediction<-glmer(recognition_accuracy~prediction_condition+(prediction_condition  | participant),
                   family = binomial, data = exp1)

summary(modexp1_prediction)

Anova(modexp1_prediction)


# accuracy now



data_agg_exp1_acc$prediction_accuracy<-ifelse(data_agg_exp1_acc$prediction_accuracy ==0, "Incorrect", 
                                              "Correct")

dat_summary_exp1_acc <- summarySEwithin(data_agg_exp1_acc,
                                        measurevar = "rec_acc",
                                        withinvars = c( "prediction_accuracy"), 
                                        idvar = "participant")

data_agg_exp1_acc$prediction_accuracy<-as.factor(data_agg_exp1_acc$prediction_accuracy)
gplot_exp1_acc<-ggplot(data_agg_exp1_acc, aes( x=prediction_accuracy, y=rec_acc))+
  geom_bar(aes(prediction_accuracy, rec_acc, fill = prediction_accuracy),
           position="dodge",stat="summary", fun.y="mean", SE=F)+
  
  geom_errorbar(aes(y = rec_acc, ymin = rec_acc - se, ymax = rec_acc + se),
                color = "black", width = 0.10, data=dat_summary_exp1_acc)+
  #facet_wrap(experiment~.)+
  geom_jitter(width = 0.20 )+
  theme_bw()+
  ylab("% Hit")+
  theme(legend.position = "none")+
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text=element_text(size=20)
  )+
  ylim(0,1)+
  xlab("Prediction Outcome")+
  ggtitle("Experiment 1")+
  theme(plot.title = element_text(hjust = 0.5))+

  scale_fill_viridis(discrete=TRUE, option = "plasma") 


gplot_exp1_acc

# save it
ggsave(paste0("/home/francesco/PowerFolders/Frankfurt_University/PIVOTAL/",
              "PREMUP_computational/computational_model/figures/prediction_acc_exp1.png"), 
       width = 7, height = 7)

# is that significant?
modexp1_acc<-glmer(recognition_accuracy~prediction_accuracy+(prediction_accuracy  | participant),
                   family = binomial, data = exp1)

summary(modexp1_acc)

Anova(modexp1_acc)

# accuracy by prediction outcome
#levels(exp1$prediction_accuracy)<-c("Incorrect", "Correct")
#exp1$prediction_accuracy<-ifelse(exp1$prediction_accuracy == 0, "Incorrect", "Correct")

exp1$prediction_accuracy<-ifelse(exp1$prediction_accuracy==0, 
                                                   "Incorrect", "Correct")
exp1$prediction_accuracy<-factor(exp1$prediction_accuracy,
                                                   ordered=F)


exp1$prediction_accuracy<-relevel(exp1$prediction_accuracy, 
                                                    "Incorrect")

data_agg_exp1_pred_acc<-exp1 %>%
  group_by(  prediction_accuracy, participant, prediction_condition) %>%
  dplyr::summarise(rec_acc = mean(recognition_accuracy, na.rm = T), 
                   experiment = first(experiment))

data_summary_exp1_pred_acc <- summarySEwithin(data_agg_exp1_pred_acc,
                                        measurevar = "rec_acc",
                                        withinvars = c( "prediction_accuracy",
                                                        "prediction_condition"), 
                                        idvar = "participant")


data_agg_exp1_pred_acc$prediction_accuracy<-factor(data_agg_exp1_pred_acc$prediction_accuracy)
data_agg_exp1_pred_acc$prediction_accuracy<-relevel(data_agg_exp1_pred_acc$prediction_accuracy, 
                                               "Incorrect")

gplot_exp1_pred_acc<-ggplot(data_agg_exp1_pred_acc, aes( x=prediction_condition, y=rec_acc))+
  geom_bar(aes(prediction_condition, rec_acc, fill = prediction_condition),
           position="dodge",stat="summary", fun.y="mean", SE=F)+
  #geom_jitter(width = 0.20, alpha = 0.40 )+
  
  geom_errorbar(aes(y = rec_acc, ymin = rec_acc - se, ymax = rec_acc + se),
                color = "black", width = 0.10, data=data_summary_exp1_pred_acc)+
  #facet_wrap(experiment~.)+
  theme_classic()+
  ylab("% Hit")+
  theme(legend.position = "none")+
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text=element_text(size=20)
  )+
  theme(strip.text.x = element_text(size = 18))+
  xlab("Contingency")+
  ggtitle("Experiment 1")+
  facet_wrap(.~prediction_accuracy)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values =   c("#117733", "#CC6677"))

gplot_exp1_pred_acc

# save it
ggsave(paste0("/Users/francescopupillo/PowerFolders/Frankfurt_University/PIVOTAL/",
              "PREMUP_computational/computational_model/figures/contingency_acc_exp1.png"), 
       width = 7, height = 7)

# analyse
modexp1_pred_acc<-glmer(recognition_accuracy~prediction_accuracy*prediction_condition+
                          (prediction_accuracy*prediction_condition  | participant),
                   family = binomial, data = exp1, glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(modexp1_pred_acc)

Anova(modexp1_pred_acc, type = "III")

library(lsmeans)

lsmeans(modexp1_pred_acc, pairwise~prediction_accuracy*prediction_condition, 
        adjust = "bonferroni")

# only accurate
modexp1_pred_acc_corr<-glmer(recognition_accuracy~prediction_condition+
                          (prediction_condition  | participant),
                        family = binomial, data = exp1[exp1$prediction_accuracy=="Correct",]
                        , glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(modexp1_pred_acc_corr)



# only inaccuractee
modexp1_pred_acc_inc<-glmer(recognition_accuracy~prediction_condition+
                               (prediction_condition  | participant),
                             family = binomial, data = exp1[exp1$prediction_accuracy=="Incorrect",]
                             , glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(modexp1_pred_acc_inc)

#------------------------------------------------------------------------


#-----------------Experiment2------------------------------------------------#
# exclude participants with low performance in phase1
exclPhase1exp2<-c(3 ,13, 28, 36, 39)

exp2<-exp2[!exp2$participant %in% exclPhase1exp2, ]




#-------------------------------------------------------------------------
# Prediction condition and prediction acc


##---------------------------------------------------------
# accuracy by prediction
data_agg_exp2_acc_pred<-exp2 %>%
  group_by(  prediction_accuracy,prediction_condition,  participant) %>%
  dplyr::summarise(rec_acc = mean(recognition_accuracy, na.rm = T), 
                   experiment = first(experiment))

dat_summary_exp2_acc_pred <- summarySEwithin(data_agg_exp2_acc_pred,
                                             measurevar = "rec_acc",
                                             withinvars = c( "prediction_accuracy", "prediction_condition"), 
                                             idvar = "participant")

# rename the levels of prediction accuracy
data_agg_exp2_acc_pred$prediction_accuracy<-as.factor(data_agg_exp2_acc_pred$prediction_accuracy)
levels(data_agg_exp2_acc_pred$prediction_accuracy)<-c("Incorrect", "Correct")

gplot_exp2_pred_acc<-ggplot(data_agg_exp2_acc_pred, aes( x=prediction_accuracy, y=rec_acc))+
  geom_bar(aes(prediction_accuracy, rec_acc, fill = prediction_accuracy),
           position="dodge",stat="summary", fun.y="mean", SE=F)+
  
  geom_errorbar(aes(y = rec_acc, ymin = rec_acc - se, ymax = rec_acc + se),
                color = "black", width = 0.10, data=dat_summary_exp2_acc_pred)+
  facet_wrap(prediction_accuracy~.)+
  theme_bw()+
  ylab("% Hit")+
  theme(legend.position = "none")+
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text=element_text(size=20),
    strip.text.x = element_text(size = 20)
  )+
  ylim(0,1)+
  xlab("Prediction Condition")+
  ggtitle("Experiment 2")+
  theme(plot.title = element_text(hjust = 0.5))+
  
  scale_fill_viridis(discrete=TRUE, option = "plasma") 

gplot_exp2_pred_acc

# save it
ggsave(paste0("/home/francesco/PowerFolders/Frankfurt_University/PIVOTAL/",
              "PREMUP_computational/computational_model/figures/prediction_acc_exp2.png"), 
       width = 10, height = 7)


#accuracy by prediction condition
data_agg_exp2_pred_acc<-exp2 %>%
  group_by( prediction_accuracy, participant, prediction_condition) %>%
  dplyr::summarise(rec_acc = mean(recognition_accuracy, na.rm = T), 
                   experiment = first(experiment))

data_summary_exp2_pred_acc <- summarySEwithin(data_agg_exp2_pred_acc,
                                              measurevar = "rec_acc",
                                              withinvars = c( "prediction_accuracy",
                                                              "prediction_condition"), 
                                              idvar = "participant")

#relevel accuracy
data_agg_exp2_pred_acc$prediction_accuracy<-factor(data_agg_exp2_pred_acc$prediction_accuracy)
data_agg_exp2_pred_acc$prediction_accuracy<-relevel(data_agg_exp2_pred_acc$prediction_accuracy, 
                                                    "Incorrect")

gplot_exp2_pred_acc<-ggplot(data_agg_exp2_pred_acc, aes( x=prediction_condition, y=rec_acc))+
  geom_bar(aes(prediction_condition, rec_acc, fill = prediction_condition),
           position="dodge",stat="summary", fun.y="mean", SE=F)+
  #geom_jitter(width = 0.20, alpha = 0.3 )+
  
  geom_errorbar(aes(y = rec_acc, ymin = rec_acc - se, ymax = rec_acc + se),
                color = "black", width = 0.10, data=data_summary_exp2_pred_acc)+
  #facet_wrap(experiment~.)+
  theme_classic()+
  ylab("% Hit")+
  theme(legend.position = "none")+
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text=element_text(size=20)
  )+
  xlab("Contingency")+
  ggtitle("Experiment 2")+
  facet_wrap(.~prediction_accuracy)+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(strip.text.x = element_text(size = 18))+
  scale_fill_manual(values =   c("#332288","#44AA99", "#AA4499"))#,"#44AA99","#332288"))

gplot_exp2_pred_acc

# save it
ggsave(paste0("/Users/francescopupillo/PowerFolders/Frankfurt_University/PIVOTAL/",
              "PREMUP_computational/computational_model/figures/contingency_acc_exp2.png"), 
       width = 10, height = 7)

# analyse
modexp2_pred_acc<-glmer(recognition_accuracy~prediction_accuracy*prediction_condition+
                          (prediction_accuracy*prediction_condition  | participant),
                        family = binomial, data = exp2, glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(modexp2_pred_acc)

Anova(modexp2_pred_acc, type = "III")

modexp2_pred<-glmer(recognition_accuracy~prediction_condition+
                          (prediction_condition  | participant),
                        family = binomial, data = exp2, glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(modexp2_pred)

library(lsmeans)

lsmeans(modexp2_pred, pairwise~prediction_condition)

lsmeans(modexp2_pred_acc, pairwise~prediction_condition)

lsmeans(modexp2_pred_acc, pairwise~prediction_accuracy*prediction_condition, 
        adjust = "bonferroni")

modexp2_pred_acc_corr<-glmer(recognition_accuracy~prediction_condition+
                          (prediction_condition  | participant),
                        family = binomial, 
                        data = exp2[exp2$prediction_accuracy==1,], 
                        glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(modexp2_pred_acc_corr)

lsmeans(modexp2_pred_acc_corr, pairwise~prediction_condition, 
        adjust = "bonferroni")

exp2$prediction_condition<-as.factor(exp2$prediction_condition)

modexp2_pred_acc_incorr<-glmer(recognition_accuracy~prediction_condition+
                               (prediction_condition  | participant),
                             family = binomial, 
                             data = exp2[exp2$prediction_accuracy==0,], 
                             glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))


lsmeans(modexp2_pred_acc_incorr, pairwise~prediction_condition, 
        adjust = "bonferroni")


