lik_OptimalBayesian_feedb<-function (
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
  # even when there is no response. All categories are updated. In addition, the learning rate is 1/t, t being the trial number for a specific scene. 
  #
  # Input
  #   Data: a long dataset where each row represents a trial. 
  #   beta: a candidate beta parameter
  #   print: 1: return only the negative log-likelihood; 
  #          2: return 1: "Negative LogLikel"; 2:"Q1"; 3:"Q2"; 4:"Q3"
  #             5: "Delta1"; 6: "Delta2"; 7: "Delta3", 8: "P1" (Probability
  #             of choosing category 1), 9:"P2", 10: "P3"
  #          3: return all the date
  #   initialQ: value of the inital Q
  #
  # Output:
  #   See "print"
  # -------------
  
  # assign trialNumber: it is the length of the row of the data file
  Data$trialN<-1:length(Data[,1]) 
  
  # Initialize variables: Qs (we may consider using the average probability of each category, but ok for now)
  Data$Q1<-NA; Data$Q2<-NA; Data$Q3<-NA
  # Ps (probabilities)
  Data$P1<-NA; Data$P2<-NA; Data$P3<-NA
  # probability strong prior (scenes 4, 5, 6, were there are things to learn)
  Data$Prob<-NA
  # Delta, prediction error
  Data$Delta<-NA
  # initiate the update : Q at time t minus Q at time t-1
  Data$update<-NA
  # initialize the variance:
  Data$variance<-NA
  # index variables for Q, P,, x, alphas,  and Delta
  Qindex<-c("Q1", "Q2", "Q3")  # this variable indexes the Q referred to each of the three categories
  Pindex<-c("P1", "P2", "P3")  # this variable indexes the P referred to each of the three categories
  xindex<-c("x1", "x2", "x3")  # this is the observation: x=1 indicates the x for the category that is presented on each trials. The other two have x = 0. 
  alphaindex<-c("alpha1", "alpha2", "alpha3") # the alphas here represent the pseudocounts for the categories at trial t. 
  Deltaindex<-c("Delta1", "Delta2", "Delta3") # Prediction error on each trial, for each cateogory. 
  
  # Counter for indicating which scene has to be updated
  count<-rep(0, 6)
  
  # initialise choice probability and counter for the choiceprobability
  prob<-NA
  count2<-1
  
  # assign information about the feedb displayed to a vector
  x<-getfeedb(Data, initialQ)
  Data[, xindex]<-x
  
  # loop over trials
  for (t in 1: max(Data$trialN)){
    
    # calculate the variance
    # number of categories
    K<-length(unique(Data$object_cat))
    # pseudocounts up to that point, for that scene (add initial pseudocount)
    if (count[Data$scene_cat[t]] ==0) { # if this is the first trial
      alpha_k<-c(1, 1, 1) # we are initialising them at 1
    } else {
      alpha_k<-base::colSums(Data[Data$scene_cat==Data$scene_cat[t], xindex], na.rm=T)+c(1,1,1)
    }
    mean_k<-alpha_k/sum(alpha_k)
    alpha_0 = sum(alpha_k)
    var_k = (alpha_k*(alpha_0-alpha_k))/ (alpha_0^2 * (alpha_0+1))
    
    Data[t, alphaindex]<-alpha_k
    
    Data$variance[t]<-sum(var_k)
    
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
      
      # update the counter 
      count2<-count2+1
    } 
    
    # in order to get the update, we need to make the difference between Q at t=1 and Q at t=t+1
    # this is the old Q
    #OldQ<-Q
    
    # update values 
    # the learning rate is 1 / t, where t is the number of times that particular scene has appeared at trial t.
    updateVal<-update(r = Data[t,xindex], Q =  Q, alpha = (1/ count[Data$scene_cat[t]]))
    
    # prediction error
    delta<-updateVal$delta
    
    # Q
    Q <-updateVal$Q
    
    # now calculate the update
    #Data$update[t]<-sum(Q-OldQ)
    
    # assign values to the dataset
    Data[t, Qindex]<-Q
    Data[t, Pindex]<-p
    Data[t, Deltaindex]<-delta
  
  }
  # take only the prob for the high prior scenes
  Probstrong<-Data$Prob[Data$scene_cat==4| Data$scene_cat==5 | Data$scene_cat==6]
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

