lik_dfLR_Instr<-function (Data,alpha, beta,print,  initialQ){
# -----------------------------------------------------------------------------#
# This function computes the likelihood of the participants'
# in this version, the values are updated depending on which category is shown
# in the end, regardles of the feedb.
# even when there is no response. All categories are updated. In addition,
# the learning rate is alpha/t, t being the trial number for a specific scene. 
#
# Input
#   Data: a long dataset where each row represents a trial. 
#   beta: a candidate beta parameter
#   print: 1: return only the negative log-likelihood; 
#          2: return all data
#   initialQ: value of the inital Q
#
# Output:
#   See "print"
# -----------------------------------------------------------------------------#


# assign trialNumber: it is the length of the row of the data file
Data$trialN<-1:length(Data[,1]) 

# initialize variables
Qindex<-vector()
Pindex<-vector()
Deltaindex<-vector()
xindex<-vector()
alphaindex<-vector()
Deltaindex<-vector()
Varianceindex<-vector()

for (i in 1:ncol(initialQ)){
  
  # Initialize variables: Qs (we may consider using the average probability 
  # of each category, but ok for now)
  Data[[paste("Q", i, sep="")]]<-NA
  
  # Ps (probabilities)
  Data[[paste("P", i, sep="")]]<-NA
  
  # variance
  Data[[paste("variance", i, sep="")]]<-NA
  
  # index variables for Q, P, and Delta
  Qindex[i]<-paste("Q", i, sep="")
  Pindex[i]<-paste("P", i, sep="")
  Deltaindex[i]<-paste("Delta", i, sep="")
  xindex[i]<-paste("x", i, sep="")
  alphaindex[i]<-paste("alpha", i, sep="")
  Varianceindex[i]<-paste("variance", i, sep="")
  
}

Data$Prob<-NA
# Delta, prediction error
Data$Delta<-NA
# initiate the update : Q at time t minus Q at time t-1
Data$update<-NA

# Counter for indicating which scene has to be updated
count<-rep(0, 6)

# initialise choice probability and counter for the choiceprobability
prob<-NA
count2<-1

# loop over trials
for (t in 1: max(Data$trialN)){
  
  # calculate the variance
  # number of categories
  K<-length(unique(Data$object_cat))
  # pseudocounts up to that point, for that scene (add initial pseudocount)
  if (count[Data$scene_cat[t]] ==0) { # if this is the first trial
    alpha_k<-rep(1, ncol(initialQ)) # we are initialising them at 1
  } else {
  alpha_k<-base::colSums(Data[Data$scene_cat==Data$scene_cat[t], xindex], 
                         na.rm=T)+rep(1, ncol(initialQ))
  }
  
  # getting the variance (from Murphy, Machine Learning). We use the pseudocounts
  mean_k<-alpha_k/sum(alpha_k)
  alpha_0 = sum(alpha_k)
  var_k = (alpha_k*(alpha_0-alpha_k))/ (alpha_0^2 * (alpha_0+1))
  
  Data[t, alphaindex]<-alpha_k
  
  Data[t, Varianceindex]<-sum(var_k)
  
  # The following loop retrieves the Q values of the scene that corresponds to 
  # the current scene (time t).
  if (count[Data$scene_cat[t]]==0){
    Q<-initialQ[Data$scene_cat[t],]  # if it is the first category of that scene, 
    # the Qs are at their initial value
  } else{
    # if it is not the first category of a scene, retrieve the Qs of the last
    # trial of that scene
    Q<-Data[Data$scene_cat==Data$scene_cat[t],][count[Data$scene_cat[t]],Qindex] 
  }
  
  count[Data$scene_cat[t]]<-count[Data$scene_cat[t]]+1 # update the counter
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
  
  # get the observation
  Data<-getobs(Data, t, xindex)
  
  delta <- Data[t,xindex]-Q # subtracting 1 to the objectcategory, 
  # the category shown at the end
  
  # in order to get the update, we need to make the difference between 
  # Q at t=1 and Q at t=t+1
  # this is the old Q
  OldQ<-Q
  
  # update the Q related to the response according to the rw model.
  # the learning rate is 1 / t, where t is the number of times that particular 
  # scene has appeared at trial t. 
  Q <-Q+ (alpha/ count[Data$scene_cat[t]]) *delta
  
  # now calculate the update
  Data$update[t]<-sum(Q-OldQ)
  
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
} else if (print==2){
  return(Data)}
}
