lik_RescorlaWagner_feedbCK<-function (Data,alpha, beta, alpha_c, beta_c, print,  initialQ, initialCK){
  # This function computes the likelihood of the participants'
  # choices conditional on the Rescorla Wagner model 
  # in this version, only probabilities for the strong prior condition are returned
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
  
  # assign trialNumber: it is the length of the row of the data file
  Data$trialN<-1:length(Data[,1]) 
  
  Qindex<-vector()
  Pindex<-vector()
  CK<-vector()
  CKindex<-vector()
  
  
  for (i in 1:ncol(initialQ)){
    
    # Initialize variables: Qs (we may consider using the average probability of each category, but ok for now)
    Data[[paste("Q", i, sep="")]]<-NA
    
    # Ps (probabilities)
    Data[[paste("P", i, sep="")]]<-NA
    
    # CK
    Data[[paste("CK", i, sep="")]]<-NA

    # index variables for Q, P, and Delta
    Qindex[i]<-paste("Q", i, sep="")
    Pindex[i]<-paste("P", i, sep="")
    CKindex[i]<-paste("CK", i, sep="")

  }
  
  # Ps (probabilities)
  Data$P1<-NA; Data$P2<-NA; Data$P3<-NA
  # probability for the choice made by participants
  Data$Prob<-NA
  # Delta, prediction error
  Data$Delta<-NA

  # index variables for Q, P, and Delta
  Qindex<-c("Q1", "Q2", "Q3")
  Pindex<-c("P1", "P2", "P3") 
  CKindex<-c("CK1", "CK2", "CK3")
  
  # Counter for indicating which scene has to be updated
  count<-rep(0, 6)
  
  Data$acc<-NA
  for (n in 1:length(Data[,1])){
    if (Data$response[n]==Data$object_cat[n]){
      Data$acc[n]<-1}else{Data$acc[n]<-0}
  }
  
  # initialise choice probability and counter for the choiceprobability
  prob<-NA
  count2<-1
  
  # loop over trials
  for (t in 1: max(Data$trialN)){
    
    # The following loop retrieves the Q values of the scene that corresponds to the current scene (time t).
    if (count[Data$scene_cat[t]]==0){
      Q<-c(initialQ, initialQ, initialQ) # if it is the first category of that scene, the Qs are at their initial value
      CK<-c(initialCK, initialCK, initialCK)
      count[Data$scene_cat[t]]<-count[Data$scene_cat[t]]+1 # update the counter
    } else{
      Q<-Data[Data$scene_cat==Data$scene_cat[t],][count[Data$scene_cat[t]],Qindex] # if it is not the first category of a scene, retrieve the Qs of the last trial of that scene
      CK<-Data[Data$scene_cat==Data$scene_cat[t],][count[Data$scene_cat[t]],CKindex] # if it is not the first category of a scene, retrieve the Qs of the last trial of that scene
      count[Data$scene_cat[t]]<-count[Data$scene_cat[t]]+1
    }
    
    # update choice probabilities using the softmax distribution
    p<-exp(beta*Q+beta_c*CK)/sum(exp(beta*Q+beta_c*CK))
    
    # compute Q, delta, and choice probability for actual choice, only if a choice is computed
    if (Data$response[t]!=0){
      
      # probability only for the response made by participant
      prob[count2]<-unlist(p[Data$response[t]])
      
      # assign it to the dataset
      Data$Prob[t]<- prob[count2]
      
      # prediction error
      delta <- Data$acc[t]- Q[Data$response[t]]
      
      Data[t, "Delta"]<-delta
      
      # update the Q related to the response according to the rw model
      Q[Data$response[t]]<-Q[Data$response[t]]+ alpha *delta
      
      # update according to choice kernel model
      CK = (1-alpha_c) * CK
      CK[Data$response[t]]<-CK[Data$response[t]]+alpha_c*1
      
      # update the counter 
      count2<-count2+1
    }
    
    # assign values to the dataset
    Data[t, Qindex]<-Q
    Data[t, Pindex]<-p
    Data[t, CKindex]<-CK
    
  }
  
  # take only the prob for the high prior scenes
  Probstrong<-getProbStrongWeak(Data, setup)
  # compute negative log-likelihood as the negative sum of log prob relative to the choice
  NegLL<--sum(log(Probstrong), na.rm=T)
  
  if (print ==1){
    return(NegLL)
  }else if ( print==2){
    return (list("Negative LogLikel"=NegLL, "Q1"= Data$Q1,"Q2"= Data$Q2,"Q3"= Data$Q3, 
                 "CK1"= Data$CK1,"CK2"= Data$CK2,"CK3"= Data$CK3,
                 "Delta"= Data$Delta,"P1"=Data$P1,"P2"= Data$P2, "P3"=Data$P3 ))
  } else if(print==3){
    return(Data)}
}

