lik_ChoiceKernel<-function (Data,alpha, beta,print,  initialCK){
  # This function computes the likelihood of the participants'
  # choices conditional on the choice kernel
  #
  # Input
  #   Data: a long dataset where each row represents a trial. 
  #   alpha: a candidate alpha parameter 
  #   beta: a candidate beta parameter
  #   print: 1: return only the negative log-likelihood; 
  #          2: return 1: "Negative LogLikel"; 2:"Q1"; 3:"Q2"; 4:"Q3"
  #          5: "Delta1"; 6: "Delta2"; 7: "Delta3", 8: "P1" (Probability
  #           of choosing category 1), 9:"P2", 10: "P3"
  #   initialCK: value of the inital CK
  #
  # Output:
  #   Negative Log Likelihood
  # -------------
  
  # assign trialNumber: it is the length of the row of the data file
  Data$trialN<-1:length(Data[,1]) 
  
  # Initialize variables: Qs (we may consider using the average probability of each category, but ok for now)
  Data$CK1<-NA; Data$CK2<-NA; Data$CK3<-NA
  # Ps (probabilities)
  Data$P1<-NA; Data$P2<-NA; Data$P3<-NA
  # Initialise variable to store the probability for the choice made by participant
  Data$Prob<-NA
  # Delta, prediction error
  Data$Delta<-NA
  
  # index variables for Q, P, and Delta
  CKindex<-c("CK1", "CK2", "CK3")
  Pindex<-c("P1", "P2", "P3") 
  # Counter for indicating which scene has to be updated
  count<-rep(0, 6)
  
  # initialise choice probability and counter for the choiceprobability
  prob<-NA
  count2<-1
  
  # loop over trials
  for (t in 1: max(Data$trialN)){
    
    # The following loop retrieves the Q values of the scene that corresponds to the current scene (time t).
    if (count[Data$scene_cat[t]]==0){
      CK<-initialCK[Data$scene_cat[t],] # if it is the first category of that scene, the Qs are at their initial value
      count[Data$scene_cat[t]]<-count[Data$scene_cat[t]]+1 # update the counter
    } else{
      CK<-Data[Data$scene_cat==Data$scene_cat[t],][count[Data$scene_cat[t]],CKindex] # if it is not the first category of a scene, retrieve the Qs of the last trial of that scene
      count[Data$scene_cat[t]]<-count[Data$scene_cat[t]]+1
    }
    
    # update choice probabilities using the softmax distribution
    p<-softmax(CK, beta)
    
    # compute Q, delta, and choice probability for actual choice, only if a choice is computed
    if (Data$response[t]!=0){
      
      # probability only for the response made by participant
      prob[count2]<-unlist(p[Data$response[t]])
      
      # assign it to the dataset
      Data$Prob[t]<- prob[count2]
      
      # update values
      CK = (1-alpha) * CK
      CK[Data$response[t]]<-CK[Data$response[t]]+alpha*1

      # update the counter 
      count2<-count2+1
    }
    
    # assign values to the dataset
    Data[t, CKindex]<-CK
    Data[t, Pindex]<-p
  }
  
  # take only the prob for the high prior scenes
  Probstrong<-getProbStrongWeak(Data, setup)
  # compute negative log-likelihood as the negative sum of log prob relative to the choice
  NegLL<--sum(log(Probstrong), na.rm=T)
  
  if (print ==1){
    return(NegLL)
  }else if ( print==2){
    return (list("Negative LogLikel"=NegLL, "CK1"= Data$CK1,"CK2"= Data$CK2,"CK3"= Data$CK3,
                 "P1"=Data$P1,"P2"= Data$P2, "P3"=Data$P3 ))
  } else if(print==3){
    return(Data)}
}

