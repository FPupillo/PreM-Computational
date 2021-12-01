lik_RescorlaWagner_obsALL1nstick<-function (Data, beta, omega, print,  initialQ, initialU){
  
# This function computes the likelihood of the participants'
# choices conditional on the Rescorla Wagner model.  
# in this version, the values are updated depending on which category is shown in the end, regardles of the feedb.
# even when there is no response. in this version we also have the sickiness parameter, and the alpha is 1 divided number of the trials. 
#
# Input
#   Data: a long dataset where each row represents a trial. 
#   alpha: a candidate alpha parameter 
#   beta: a candidate beta parameter
#   omega: a condidate omega
#   print: 1: return only the negative log-likelihood; 
#          2: return 1: "Negative LogLikel"; 2:"Q1"; 3:"Q2"; 4:"Q3"
#          5: "Delta1"; 6: "Delta2"; 7: "Delta3", 8: "P1" (Probability
#           of choosing category 1), 9:"P2", 10: "P3"
#   initialQ: value of the inital Q
#   initialU: initial value of the U
#  
# Output:
#   Negative Log Likelihood
# -------------

# assign trialNumber: it is the length of the row of the data file
Data$trialN<-1:length(Data[,1]) 

# initialize variables
Qindex<-vector()
Pindex<-vector()
Uindex<-vector()
Deltaindex<-vector()

for (i in 1:ncol(initialQ)){
  
  # Initialize variables: Qs (we may consider using the average probability of each category, but ok for now)
  Data[[paste("Q", i, sep="")]]<-NA
  
  # Ps (probabilities)
  Data[[paste("P", i, sep="")]]<-NA
  
  # Us
  Data[[paste("U", i, sep="")]]<-NA
  
  # index variables for Q, P, and Delta
  Qindex[i]<-paste("Q", i, sep="")
  Pindex[i]<-paste("P", i, sep="")
  Uindex[i]<-paste("U", i, sep="")
  Deltaindex[i]<-paste("Delta", i, sep="")
  
}

# probability strong prior
Data$Prob<-NA
# Delta, prediction error
Data$Delta<-NA

# Counter for indicating which scene has to be updated
count<-rep(0, 6)

# initialise choice probability and counter for the choiceprobability
prob<-NA
count2<-1

# assign information about the category displayed to a vector
x<-getx(Data)

# loop over trials
for (t in 1: max(Data$trialN)){
  
  # The following loop retrieves the Q values of the scene that corresponds to the current scene (time t).
  if (count[Data$scene_cat[t]]==0){
    Q<-initialQ[Data$scene_cat[t],]  # if it is the first category of that scene, the Qs are at their initial value
    U<-initialU[Data$scene_cat[t],] 
    count[Data$scene_cat[t]]<-count[Data$scene_cat[t]]+1 # update the counter
  } else{
    Q<-Data[Data$scene_cat==Data$scene_cat[t],][count[Data$scene_cat[t]],Qindex] # if it is not the first category of a scene, retrieve the Qs of the last trial of that scene
    #U<-Data[Data$scene_cat==Data$scene_cat[t],][count[Data$scene_cat[t]],Uindex] # if it is not the first category of a scene, retrieve the Qs of the last trial of that scene
    U<-Data[t-1, Uindex] # this is the previous choice, regardless of the scene
    count[Data$scene_cat[t]]<-count[Data$scene_cat[t]]+1
  }
  
  # update choice probabilities using the softmax distribution
  p<-exp(beta*Q+omega*U)/sum(exp(beta*Q+omega*U))
  
  # compute Q, delta, and choice probability for actual choice
  if (Data$response[t]!=0){
    
    # probability only for the response made by participant
    prob[count2]<-unlist(p[Data$response[t]])
    
    
    # assign it to the dataset
    Data$Prob[t]<- prob[count2]
    
    # update the counter 
    count2<-count2+1
    
    # update stickiness
    # the stickiness depends on the choice
    U<-getU(Data, initialQ,t)
    
  } 
  
  
  delta <- x[t,]-Q # subtracting 1 to the objectcategory, the category shown at the end
  
  # update the Q related to the response according to the rw model. the learning rate is 1 / t, where t is the number of times that particular scene has appeared at trial t. 
  Q <-Q+ (1/ count[Data$scene_cat[t]])*delta

  
  # assign values to the dataset
  Data[t, Qindex]<-Q
  Data[t, Uindex]<-U
  Data[t, Pindex]<-p
  Data[t, Deltaindex]<-delta
  
  
}
# take only the prob for the high prior scenes
Probstrong<-getProbStrongWeak(Data, setup)
# compute negative log-likelihood as the negative sum of log prob relative to the choice
NegLL<--sum(log(Probstrong), na.rm=T)

if (print ==1){
  return(NegLL)
}else if ( print==2){
  return (list("Negative LogLikel"=NegLL, "Q1"= Data$Q1,"Q2"= Data$Q2,"Q3"= Data$Q3,"U1"= Data$U1,"U2"= Data$U2,"U3"= Data$U3,
               "Delta1"= Data$Delta1,"Delta2"= Data$Delta2,"Delta3"= Data$Delta3, "P1"=Data$P1,"P2"= Data$P2, "P3"=Data$P3 ))
} else if(print==3){
  return(Data)}
}

