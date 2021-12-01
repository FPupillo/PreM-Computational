lik_Dirichlet<-function (Data,alpha,print){
  
# This function computes the likelihood of the participants'
# choices conditional on Dirichlet model

#
# Input
#   Data: a long dataset where each row represents a trial. 
#   alpha: a candidate alpha parameter 
#   beta: a candidate beta parameter
#   print: 1: return only the negative log-likelihood; 
#          2: return 1: "Negative LogLikel"; 2:"Q1"; 3:"Q2"; 4:"Q3"
#          5: "Delta1"; 6: "Delta2"; 7: "Delta3", 8: "P1" (Probability
#           of choosing category 1), 9:"P2", 10: "P3"
#
# Output:
#   Negative Log Likelihood
# -------------

# assign trialNumber: it is the length of the row of the data file
Data$trialN<-1:length(Data[,1]) 

# Initialize variables: Qs (we may consider using the average probability of each category, but ok for now)
Data$alpha1<-NA; Data$alpha2<-NA; Data$alpha3<-NA
Data$alpha1post<-NA; Data$alpha2post<-NA; Data$alpha3post<-NA

# SIs (shannon)
Data$P1<-NA; Data$P2<-NA; Data$P3<-NA
Data$post1<-NA; Data$post2<-NA; Data$post3<-NA

# probability strong prior
Data$Prob<-NA
# SI, prediction error
Data$SI<-NA
Data$SI1<-NA; Data$SI2<-NA; Data$SI3<-NA

# expected probabilities
expected<-matrix(NA, nrow = nrow(Data)+1 ,ncol = 3)
# count
c<-matrix(NA, nrow = nrow(Data),ncol = 3)


# index variables for Q, P, and Delta
Alphaindex<-c("alpha1", "alpha2", "alpha3")
Alphaindexpost<-c("alpha1post", "alpha2post", "alpha3post")
Pindex<-c("P1", "P2", "P3") 
SIindex<-c("SI1", "SI2", "SI3")
posterior<-c("post1", "post2", "post3")

# Counter for indicating which scene has to be updated
count<-rep(0, 6)

# initialise choice probability and counter for the choiceprobability
prob<-NA
prior<-NA
count2<-1

# # assign information about the category displayed to a vector
# x<-matrix(NA, nrow=nrow(Data), ncol=3)
# for (n in 1:length(Data$object_cat)){
#  if (Data$object_cat[n]==1){
#   x[n,]<-c(1,0,0)
# } else if ((Data$object_cat[n]==2)){
#   x[n,]<-c(0,1,0)
# } else if (Data$object_cat[n]==3){
#   x[n,]<-c(0,0,1)}
# }

# initial prior
Data[1, Pindex]<- rep(alpha, 3)/sum(rep(alpha,3))


# loop over trials
for (t in 1: max(Data$trialN)){
  
  # The following loop retrieves the Q values of the scene that corresponds to the current scene (time t).
  if (count[Data$scene_cat[t]]==0){
    alphas<-rep(alpha, 3)  # if it is the first category of that scene, the Qs are at their initial value
    prior<-c(0.33, 0.33, 0.33)
    count[Data$scene_cat[t]]<-count[Data$scene_cat[t]]+1 # update the counter
  } else{
    alphas<-Data[Data$scene_cat==Data$scene_cat[t],][count[Data$scene_cat[t]],Alphaindexpost] # if it is not the first category of a scene, retrieve the Qs of the last trial of that scene
    prior<-(Data[Data$scene_cat==Data$scene_cat[t],][count[Data$scene_cat[t]],posterior])
    count[Data$scene_cat[t]]<-count[Data$scene_cat[t]]+1
  }
  
  # update choice probabilities using the softmax distribution
  #p<-softmax(prior, beta)
  
  
  if (Data$object_cat[t]==1){
    c[t,]<-c(1,0,0)
  } else if ((Data$object_cat[t]==2)){
    c[t,]<-c(0,1,0)
  } else if (Data$object_cat[t]==3){
    c[t,]<-c(0,0,1)}
  
  # compute Q, delta, and choice probability for actual choice
  if (Data$response[t]!=0){
    
    # probability only for the response made by participant
    prob[count2]<-unlist(prior[Data$response[t]])
    
    # assign it to the dataset
    Data$Prob[t]<- prob[count2]
    
    # update the counter 
    count2<-count2+1
  } 
  
  Data$SI[t]<- unlist(-log(prior[Data$object_cat[t]]))

  # assign values to the dataset
  Data[t, Alphaindex]<-alphas
  Data[t, Pindex]<-prior
  Data[t, SIindex]<--log(prior)
  
  # now update the posterior
  Data[t,posterior]<-unlist((alphas+colSums(c, na.rm=T))/(sum(colSums(c, na.rm=T))+sum(alphas,na.rm=T)))
  #update the alphas
  Data[t, Alphaindexpost]<-alphas+c[t,]
  # update the posterior distribution
}
# take only the prob for the high prior scenes
Probstrong<-Data$Prob[Data$scene_cat==4| Data$scene_cat==5 | Data$scene_cat==6]
# compute negative log-likelihood as the negative sum of log prob relative to the choice
NegLL<--sum(log(Probstrong), na.rm=T)

if (print ==1){
  return(NegLL)
}else if ( print==2){
  return (list("Negative LogLikel"=NegLL, "alpha1"= Data$Q1,"alpha2"= Data$Q2,"alpha3"= Data$Q3,
               "SI1"= Data$Delta1,"SI2"= Data$Delta2,"SI3"= Data$Delta3, "P1"=Data$P1,"P2"= Data$P2, "P3"=Data$P3 ))
} else if(print==3){
  return(Data)}
}

