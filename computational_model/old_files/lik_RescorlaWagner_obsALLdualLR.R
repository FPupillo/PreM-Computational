lik_RescorlaWagner_obsALLdualLR<-function (
  Data,
  alpha = NULL,
  beta = NULL,
  omega = NULL,
  alpha_o = NULL,
  beta_o = NULL, 
  print,  
  initialCK = NULL, 
  initialQ = NULL,
  initialQ_o = NULL){
  
# This function computes the likelihood of the participants'
# choices conditional on the Rescorla Wagner model.  
# in this version, the values are updated depending on which category is shown in the end, regardles of the feedb.
# even when there is no response
#
# Input
#   Data: a long dataset where each row represents a trial. 
#   alpha: a candidate alpha parameter 
#   beta: a candidate beta parameter
#   print: 1: return only the negative log-likelihood; 
#          2: return 1: "Negative LogLikel"; 2:"Q1"; 3:"Q2"; 4:"Q3"
#          5: "Delta1"; 6: "Delta2"; 7: "Delta3", 8: "P1" (Probability
#           of choosing category 1), 9:"P2", 10: "P3"
#   initialQ: value of the inital Q
#
# Output:
#   Negative Log Likelihood
# -------------

alphapos<-alpha
alphaneg<-alpha_o
  
# assign trialNumber: it is the length of the row of the data file
Data$trialN<-1:length(Data[,1]) 

Qindex<-vector()
Pindex<-vector()
Deltaindex<-vector()
Pindex<-vector()

for (i in 1:ncol(initialQ)){
  
  # Initialize variables: Qs (we may consider using the average probability of each category, but ok for now)
  Data[[paste("Q", i, sep="")]]<-NA
  
  # Ps (probabilities)
  Data[[paste("P", i, sep="")]]<-NA
  
  # index variables for Q, P, and Delta
  Qindex[i]<-paste("Q", i, sep="")
  Pindex[i]<-paste("P", i, sep="")
  Deltaindex[i]<-paste("Delta", i, sep="")
  
}
  
# probability strong prior
Data$Prob<-NA
# Delta, prediction error
Data$Delta<-NA

# assign information about the category displayed to a vector
x<-getx(Data, initialQ)
#Data[, xindex]<-x

# Counter for indicating which scene has to be updated
count<-rep(0, 6)

# initialise choice probability and counter for the choiceprobability
prob<-NA
count2<-1

# loop over trials
for (t in 1: max(Data$trialN)){
  
  # The following loop retrieves the Q values of the scene that corresponds to the current scene (time t).
  if (count[Data$scene_cat[t]]==0){
    Q<-initialQ[Data$scene_cat[t],]  # if it is the first category of that scene, the Qs are at their initial value
  } else{
    Q<-Data[Data$scene_cat==Data$scene_cat[t],][count[Data$scene_cat[t]],Qindex] # if it is not the first category of a scene, retrieve the Qs of the last trial of that scene
  }
  
  # update the counter
  count[Data$scene_cat[t]]<-count[Data$scene_cat[t]]+1 
  
  # update choice probabilities using the softmax distribution
  p<-softmax(Q, beta)
  
  # compute Q, delta, and choice probability for actual choice
  if (Data$response[t]!=0){
    
    # probability only for the response made by participant
    prob[count2]<-unlist(p[Data$response[t]])
    
    # assign it to the dataset
    Data$Prob[t]<- prob[count2]

    # get the learning rate depending on PE
    if (Data$acc[t]==1){
      lr<-alphapos
    }else{
      lr<-alphaneg
    }
    
    # update the counter 
    count2<-count2+1
    
    # update values
    # the prediction error is 1 - the Q value for that category at time t
    updateVal<-update(r = x[t,] , Q = Q, alpha = lr)
  
    # prediction error
    delta<-updateVal$delta
    
    # update the Q 
    Q<-updateVal$Q
    
  } else{

  # if it is smaller than 0
    updateVal<-update(r = x[t,], Q = Q, alpha = alphaneg)
    
    Q <-updateVal$Q
    
    delta<-updateVal$delta
    
  }
  # assign values to the dataset
  Data[t, Qindex]<-Q
  Data[t, Pindex]<-p
  Data[t, Deltaindex]<-delta

}
# take only the prob for the high prior scenes
Probstrong<-getProbStrongWeak(Data, Args[4])

# compute negative log-likelihood as the negative sum of log prob relative to the choice
NegLL<--sum(log(Probstrong), na.rm=T)

if (print ==1){
  return(NegLL)
}else if ( print==2){
  return (list("Negative LogLikel"=NegLL, "Q1"= Data$Q1,"Q2"= Data$Q2,"Q3"= Data$Q3,
               "Delta1"= Data$Delta1,"Delta2"= Data$Delta2,"Delta3"= Data$Delta3, "P1"=Data$P1,"P2"= Data$P2, "P3"=Data$P3 ))
} else if(print==3){
  return(Data)}
}

