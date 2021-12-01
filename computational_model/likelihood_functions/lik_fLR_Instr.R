lik_fLR_Instr<-function (Data,alpha, beta,print,  initialQ){
  
#------------------------------------------------------------------------------#
# This function computes the likelihood of the participants'.
# in this version, the values are updated depending on which category 
# is shown in the end, regardles of the feedb.
# even when there is no response
#
# INPUT:
#   Data: a long dataset where each row represents a trial. 
#   alpha: a candidate alpha parameter 
#   beta: a candidate beta parameter
#   print: 1: return only the negative log-likelihood; 
#          2: return all data
#   initialQ: value of the inital Q
#
# Output:
#   Negative Log Likelihood
#------------------------------------------------------------------------------#
  
# assign trialNumber: it is the length of the row of the data file
Data$trialN<-1:length(Data[,1]) 

Qindex<-vector()
# calculate the Qs before the evidence is shown
QbefIndex<-vector()
Deltaindex<-vector()
Varianceindex<-vector()
xindex<-vector()
Pindex<-vector()
alphaindex<-vector()


for (i in 1:ncol(initialQ)){
  
  # Initialize variables: Qs (we may consider using the average probability
  # of each category, but ok for now)
  Data[[paste("Q", i, sep="")]]<-NA
  
  # Qs before
  Data[[paste("befQ", i, sep="")]]<-NA
  
  # Ps (probabilities)
  Data[[paste("P", i, sep="")]]<-NA

  Data[[paste("variance", i, sep="")]]<-NA
  
  # index variables for Q, P, and Delta
  Qindex[i]<-paste("Q", i, sep="")
  QbefIndex[i]<-paste("befQ", i, sep="")
  Pindex[i]<-paste("P", i, sep="")
  Deltaindex[i]<-paste("Delta", i, sep="")
  xindex[i]<-paste("x", i, sep="")
  Varianceindex[i]<-paste("variance", i, sep="")
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
x<-getx(Data, initialQ)
Data[, xindex]<-x

# get the observation
#Data<-getobs(Data, t, xindex)

# loop over trials
for (t in 1: max(Data$trialN)){
  
  # get the variance
  Data<-var_murphy(Data,count,t , alphaindex, Varianceindex, xindex)
  
  # The following loop retrieves the Q values of the scene that corresponds to 
  # the current scene (time t).
  if (count[Data$scene_cat[t]]==0){
    Q<-initialQ[Data$scene_cat[t],]  # if it is the first category of that scene,
    # the Qs are at their initial value
  } else{
    Q<-Data[Data$scene_cat==Data$scene_cat[t],][count[Data$scene_cat[t]],Qindex] 
    # if it is not the first category of a scene, retrieve the Qs of the last
    # trial of that scene
  }
  
  count[Data$scene_cat[t]]<-count[Data$scene_cat[t]]+1
  
  # assign the Qs to the dataset
  Data[t, QbefIndex]<-Q
  
  # update choice probabilities using the softmax distribution
  p<-softmax(Q, beta)
  

  # compute Q, delta, and choice probability for actual choice
  if (Data$response[t]!=0){
    
    # probability only for the response made by participant
    prob[count2]<-unlist(p[Data$response[t]])
    
    # assign it to the dataset
    Data$Prob[t]<- prob[count2]
    
    # update the counter 
    count2<-count2+1
  } 
  
  # the category shown at the end
  updateVal<-update(r = Data[t,xindex], Q = Q, alpha = alpha)
  
  delta <- updateVal$delta # subtracting 1 to the objectcategory, 
  # the category shown at the end
  
  # update the Qs related to the response according to the rw model
  Q <-updateVal$Q
  # assign values to the dataset
  Data[t, Qindex]<-Q
  Data[t, Pindex]<-p
  Data[t, Deltaindex]<-delta
  
  
}

# take only the prob for the high prior scenes
Probstrong<-getProbStrongWeak(Data, setup)
# compute negative log-likelihood as the negative sum of log prob relative 
# to the choice
NegLL<--sum(log(Probstrong), na.rm=T)

if (print ==1){
  return(NegLL)
}else if(print==2){
  return(Data)}

}
