#------------------------------------------------------------------------------#
# Model-free analysis
# Cerated: "Tue Jan 25 14:00:40 2022"
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
source("computational_model/helper_functions/getPredicted.R")

# get wd
cd<-getwd()

# load the data of the two experiments
exp1<-fread("exp1/outputs/group_level/share/group_task-rec.csv")

#------------------Experiment1-------------------------------------------------#
# select only immediate
exp1<-exp1[exp1$session==1,]

# get predicted category
exp1<-getPredicted(exp1, 1)

# rename fillers for old (as they were NA)
exp1$fillers[exp1$OvsN==2] <- 0

# Remove fillers
exp1 <- exp1[exp1$fillers!=1,]


#------------------Experiment2-------------------------------------------------#
exp2<-fread("exp2/outputs/group_level/share/group_task-rec.csv")

# get predicted category
exp2<-getPredicted(exp2, 2)

#The data file contains a lot of information that we will not need now.
exp2 <- subset(x = exp2,
               subset = !is.na(exp2$participant.y),
               select = c("practice", "participant.y", "OvsN",  "id_acc", 
                          "trial_acc", "contingency", "fillers",  "session",
                          "predicted_category" , "predicted_contingency"))

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


# now calculate the hit rate by accuracy
#-----------------hitbyAcc------------------------------------------------#
names(exp1)
names(exp2)
# select only old 
exp1<-exp1[exp1$OvsN==1,]
exp2<-exp2[exp2$OvsN==1,]

# create prediction strength
exp1$prediction_condition<-as.factor(exp1$contingency)
exp1$prediction_condition2<- ifelse(exp1$prediction_condition=="0,33", "0.33",
                                    "0.20/0.80")
levels(exp1$prediction_condition)<-c("0.20", "0.33", "0.80")


exp2$prediction_condition<-as.factor(exp2$contingency)
exp2$prediction_condition2<- ifelse(exp2$prediction_condition==0.50, "0.50", 
                                    ifelse(exp2$prediction_condition==0.1 | 
                                            exp2$prediction_condition==0.90, 
                                           "0.10/0.90",
                                           "0.30/0.70"))

levels(exp2$prediction_condition)<-c("0.10", "0.30", "0.50", "0.70", "0.90")

# we only need encoding accuracy and 
VoI1<-c("participant", "predicted_contingency", "prediction_condition",
        "prediction_condition2", "enc_acc", "id_acc")

exp1<-exp1[, ..VoI1]

names(exp1)[c(5,6)]<-c("prediction_accuracy", "recognition_accuracy")

VoI2<-c("participant", "predicted_contingency","prediction_condition", 
        "prediction_condition2", "trial_acc", "id_acc")

exp2<-exp2[, ..VoI2]

names(exp2)[c(5,6)]<-c("prediction_accuracy", "recognition_accuracy")

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

# rename the levels of prediction accuracy
exp1$prediction_accuracy<-ifelse(exp1$prediction_accuracy==0, "Incorrect",
                                                         "Correct")
# turn them into a factor
exp1$prediction_accuracy<-as.factor(exp1$prediction_accuracy)

# make "incorrect" the first level
exp1$prediction_accuracy<-relevel(exp1$prediction_accuracy, 
                                                    "Incorrect")
# aggregate 
data_agg_exp1_pred_acc<-exp1 %>%
  group_by(  prediction_accuracy, participant, prediction_condition) %>%
  dplyr::summarise(rec_acc = mean(recognition_accuracy, na.rm = T), 
                   experiment = first(experiment))

# get within-participants' errors and CI
data_summary_exp1_pred_acc <- summarySEwithin(data_agg_exp1_pred_acc,
                                        measurevar = "rec_acc",
                                        withinvars = c( "prediction_accuracy",
                                                        "prediction_condition"), 
                                        idvar = "participant")

# plot
gplot_exp1_pred_acc<-ggplot(data_agg_exp1_pred_acc,
                            aes( x=prediction_condition, y=rec_acc))+
  geom_bar(aes(prediction_condition, rec_acc, fill = prediction_condition),
           position="dodge",stat="summary", fun.y="mean", SE=F)+
  geom_jitter(width = 0.20, alpha = 0.3 )+
  
  geom_errorbar(aes(y = rec_acc, ymin = rec_acc - se, ymax = rec_acc + se),
                color = "black", width = 0.10, data=data_summary_exp1_pred_acc)+
  #facet_wrap(experiment~.)+
  theme_classic()+
  ylab("% Hit")+
  theme(legend.position = "none")+
  theme(
    plot.title = element_text(size = 30),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    axis.text=element_text(size=28)
  )+
  theme(strip.text.x = element_text(size = 28))+
  xlab("Contingency")+
  ggtitle("Experiment 1")+
  facet_wrap(.~prediction_accuracy)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values =   c("#DDCC77", "#CC6677","#117733"))

gplot_exp1_pred_acc

# save it
ggsave("computational_model/figures/contingency_acc_exp1.png", 
       width = 10, height = 10)

# analyse
modexp1_pred_acc<-glmer(recognition_accuracy~
                     prediction_accuracy*prediction_condition+
                    (prediction_accuracy*prediction_condition | participant),
                     family = binomial, data = exp1,
                     glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(modexp1_pred_acc)

Anova(modexp1_pred_acc, type = "III")

library(lsmeans)

lsmeans(modexp1_pred_acc, pairwise~prediction_accuracy*prediction_condition, 
        adjust = "bonferroni")

#------------------------------------------------------------------------------#
# as a function of predicted contingency
data_agg_exp1_pred_cont<-exp1 %>%
  group_by(  prediction_accuracy, participant, predicted_contingency) %>%
  dplyr::summarise(rec_acc = mean(recognition_accuracy, na.rm = T), 
                   experiment = first(experiment))

data_summary_exp1_pred_cont<- summarySEwithin(data_agg_exp1_pred_cont,
                                          measurevar = "rec_acc",
                                          withinvars = c( "prediction_accuracy",
                                                      "predicted_contingency"), 
                                              idvar = "participant")


gplot_exp1_pred_cont<-ggplot(data_agg_exp1_pred_cont, aes( x=predicted_contingency, y=rec_acc))+
  geom_bar(aes(predicted_contingency, rec_acc, fill = predicted_contingency),
           position="dodge",stat="summary", fun.y="mean", SE=F)+
  geom_jitter(width = 0.20, alpha = 0.40 )+
  
  geom_errorbar(aes(y = rec_acc, ymin = rec_acc - se, ymax = rec_acc + se),
                color = "black", width = 0.10, data=data_summary_exp1_pred_cont)+
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
  xlab("Predicted Contingency")+
  ggtitle("Experiment 1")+
  facet_wrap(.~prediction_accuracy)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values =   c("#DDCC77", "#CC6677","#117733"))


gplot_exp1_pred_cont


#-----------------Experiment2------------------------------------------------#
# exclude participants with low performance in phase1
exclPhase1exp2<-c(3 ,13, 28, 36, 39)

exp2<-exp2[!exp2$participant %in% exclPhase1exp2, ]

# rename the levels of prediction accuracy
exp2$prediction_accuracy<-ifelse(exp2$prediction_accuracy==0, "Incorrect",
                                 "Correct")
# turn them into a factor
exp2$prediction_accuracy<-as.factor(exp2$prediction_accuracy)

# make "incorrect" the first level
exp2$prediction_accuracy<-relevel(exp2$prediction_accuracy, 
                                  "Incorrect")

##--------------------------------------------------------

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

data_agg_exp2_pred_acc$prediction_accuracy<-relevel(data_agg_exp2_pred_acc$prediction_accuracy, 
                                                    "Incorrect")

gplot_exp2_pred_acc<-ggplot(data_agg_exp2_pred_acc, aes( x=prediction_condition, y=rec_acc))+
  geom_bar(aes(prediction_condition, rec_acc, fill = prediction_condition),
           position="dodge",stat="summary", fun.y="mean", SE=F)+
  geom_jitter(width = 0.20, alpha = 0.3 )+
  
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
  scale_fill_manual(values =   c("#DDCC77","#88CCEE", "#AA4499","#44AA99","#332288"))

gplot_exp2_pred_acc

# save it
ggsave("computational_model/figures/contingency_acc_exp2.png", 
       width = 7, height = 7)

# analyse
modexp2_pred_acc<-glmer(recognition_accuracy~prediction_accuracy*prediction_condition+
                          (prediction_accuracy*prediction_condition  | participant),
                        family = binomial, data = exp2, glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(modexp2_pred_acc)

Anova(modexp2_pred_acc, type = "II")

library(lsmeans)

lsmeans(modexp2_pred_acc, pairwise~prediction_condition)

lsmeans(modexp2_pred_acc, pairwise~prediction_accuracy*prediction_condition, 
        adjust = "bonferroni")

# -----------------------------------------------------------------------------#
# as a function of predicted contingency
data_agg_exp2_pred_cont<-exp2 %>%
  group_by(  prediction_accuracy, participant, predicted_contingency) %>%
  dplyr::summarise(rec_acc = mean(recognition_accuracy, na.rm = T), 
                   experiment = first(experiment))

data_summary_exp2_pred_cont<- 
  summarySEwithin(data_agg_exp2_pred_cont,
                  measurevar = "rec_acc",
                  withinvars = c( "prediction_accuracy",
                                  "predicted_contingency"), 
                  idvar = "participant")


gplot_exp2_pred_cont<-ggplot(data_agg_exp2_pred_cont, aes( x=predicted_contingency, y=rec_acc))+
  geom_bar(aes(predicted_contingency, rec_acc, fill = predicted_contingency),
           position="dodge",stat="summary", fun.y="mean", SE=F)+
  geom_jitter(width = 0.20, alpha = 0.40 )+
  
  geom_errorbar(aes(y = rec_acc, ymin = rec_acc - se, ymax = rec_acc + se),
                color = "black", width = 0.10, data=data_summary_exp2_pred_cont)+
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
  xlab("Predicted Contingency")+
  ggtitle("Experiment 2")+
  facet_wrap(.~prediction_accuracy)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values =   c("#DDCC77","#88CCEE", "#AA4499","#44AA99","#332288"))

gplot_exp2_pred_cont

# save it
ggsave("computational_model/figures/pred_contingency_acc_exp2.png", 
       width = 7, height = 7)

# analyse
modexp2_pred_acc<-glmer(recognition_accuracy~prediction_accuracy*prediction_condition+
                          (prediction_accuracy*prediction_condition  | participant),
                        family = binomial, data = exp2, glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(modexp2_pred_acc)

Anova(modexp2_pred_acc, type = "II")

library(lsmeans)

lsmeans(modexp2_pred_acc, pairwise~prediction_condition)

lsmeans(modexp2_pred_acc, pairwise~prediction_accuracy*prediction_condition, 
        adjust = "bonferroni")
