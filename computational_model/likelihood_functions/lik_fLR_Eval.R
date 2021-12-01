lik_fLR_Eval<-function (Data,alpha, beta,print,  initialQ){
  
  #----------------------------------------------------------------------------#
  # This function computes the likelihood of the participants'
  # in this version, the values are updated depending on the response 
  #
  # Input
  #   Data: a long dataset where each row represents a trial. 
  #   alpha: a candidate alpha parameter 
  #   beta: a candidate beta parameter
  #   print: 1: return only the negative log-likelihood; 
  #          2: return all data
  #   initialQ: value of the inital Q
  #
  # Output:
  #   Negative Log Likelihood
  #
  #----------------------------------------------------------------------------#
  
  # assign trialNumber: it is the length of the row of the data file
  Data$trialN<-1:length(Data[,1]) 
  
  Qindex<-vector()
  Pindex<-vector()
  Deltaindex<-vector()
  Pindex<-vector()
  x<-getx(Data, initialQ)
  
  for (i in 1:ncol(initialQ)){
    
    # Initialize variables: Qs (we may consider using the average probability 
    # of each category, but ok for now)
    Data[[paste("Q", i, sep="")]]<-NA
    
    # Ps (probabilities)
    Data[[paste("P", i, sep="")]]<-NA
    
    # index variables for Q, P, and Delta
    Qindex[i]<-paste("Q", i, sep="")
    Pindex[i]<-paste("P", i, sep="")
    Deltaindex[i]<-paste("Delta", i, sep="")
    
    
  }
  
  # probability for the response made by participant
  Data$Prob<-NA
  # Delta, prediction error
  Data$Delta<-NA
  
  
  # Counter for indicating which scene has to be updated
  count<-rep(0, 6)
  
  # initialise choice probability and counter for the choiceprobability
  prob<-NA
  count2<-1
  
  # loop over trials
  for (t in 1: nrow(Data)){
    
    # The following loop retrieves the Q values of the scene that corresponds to
    # the current scene (time t).
    if (count[Data$scene_cat[t]]==0){
      Q<-initialQ[Data$scene_cat[t],]  # if it is the first category 
      # of that scene, the Qs are at their initial value
    } else{
      Q<-Data[Data$scene_cat==Data$scene_cat[t],][count[Data$scene_cat[t]],
                                                  Qindex] # if it is not the 
      # first category of a scene, retrieve the Qs of the last trial of that scene
    }
    
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
      
      # feedback
      # get the learning rate depending on PE
      if (Data$acc[t]==1){
        r<-1
      }else{
        r<-0
      }
      # date rule
      updateVal<-update(r = r, Q = Q[Data$response[t]], alpha = alpha)
      
      Q[Data$response[t]]<-updateVal$Q 
      # PE
      delta<-updateVal$delta
      Data[t, "Delta"]<-delta
    }

    # assign values to the dataset
    Data[t, Qindex]<-Q
    Data[t, Pindex]<-p
    #  Data[t, Deltaindex]<- x[t,] - Q  # prediction error
    
  }
  
  #take only the prob for the high prior scenes
  Probstrong<-getProbStrongWeak(Data, setup)
  # compute negative log-likelihood as the negative sum of log prob relative
  # to the choice
  NegLL<--sum(log(Probstrong), na.rm=T)
  
  if (print ==1){
    return(NegLL)
  } else if(print==2){
    return(Data)}
}

