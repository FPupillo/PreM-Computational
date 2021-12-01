lik_RescorlaWagner_feedbAndObsweight<-function (Data,alpha, alpha_o, mu, omega, print,  initialQ, initialQ_o){
  # This function computes the likelihood of the participants'
  # choices conditional on the Rescorla Wagner model 
  # in this version, only probabilities for the strong prior condition are returned
  #
  # Input
  #   T<-length of the trials
  #   mu<- probability of the association
  #   alpha<- alpha parameter for the RW model updated with feedb
  #   beta<- beta parameter the RW model updated with feedback
  #   alpha_o<- alpha parameter for the RWmodel updated with the category
  #   beta_o<- beta parameter for the RWmodel updated with the category
  #   print: 1: return only the negative log-likelihood; 
  #          2: return 1: "Negative LogLikel"; 2:"Q1"; 3:"Q2"; 4:"Q3"
  #          5: "Delta1"; 6: "Delta2"; 7: "Delta3", 8: "P1" (Probability
  #           of choosing category 1), 9:"P2", 10: "P3"
  #   initalQ<- values at which the Qs for the RW model with feedback are initialised
  #   initalQ_o<- values at which the Qs for the RW model updated with the category are initialised
  #
  # Output:
  #   Negative Log Likelihood
  # -------------
  
  # assign trialNumber: it is the length of the row of the data file
  Data$trialN<-1:length(Data[,1]) 
  
  Qindex<-vector()
  Pindex<-vector()
  Delta_o_index<-vector()
  Q_oindex<-vector()
  for (i in 1:ncol(initialQ)){
    
    # Initialize variables: Qs (we may consider using the average probability of each category, but ok for now)
    Data[[paste("Q", i, sep="")]]<-NA
    
    # Ps (probabilities)
    Data[[paste("P", i, sep="")]]<-NA
    
    # Us
    Data[[paste("Q", i, "_o", sep="")]]<-NA
    
    # index variables for Q, P, and Delta
    Qindex[i]<-paste("Q", i, sep="")
    Pindex[i]<-paste("P", i, sep="")
    Delta_o_index[i]<-paste("Delta_o", i, sep="")
    Q_oindex[i]<-paste("Q", i, "_o",sep="")
    
  }

  # probability for the choice made by participant
  Data$Prob<-NA
  # Delta, prediction error
  Data$Delta<-NA

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
  
  # assign information about the category displayed to a vector
  x<-getx(Data)
  
  # loop over trials
  for (t in 1: max(Data$trialN)){
    
    # The following loop retrieves the Q values of the scene that corresponds to the current scene (time t).
    if (count[Data$scene_cat[t]]==0){
      Q<-initialQ[Data$scene_cat[t],] # if it is the first category of that scene, the Qs are at their initial value
      Q_o<-initialQ_o[Data$scene_cat[t],] # if it is the first category of that scene, the Qs are at their initial value
      count[Data$scene_cat[t]]<-count[Data$scene_cat[t]]+1 # update the counter
    } else{
      Q<-Data[Data$scene_cat==Data$scene_cat[t],][count[Data$scene_cat[t]],Qindex] # if it is not the first category of a scene, retrieve the Qs of the last trial of that scene
      Q_o<-Data[Data$scene_cat==Data$scene_cat[t],][count[Data$scene_cat[t]],Q_oindex] # if it is not the first category of a scene, retrieve the Qs of the last trial of that scene
      count[Data$scene_cat[t]]<-count[Data$scene_cat[t]]+1
    }
    
    # the preference m is given by the weighted sum of the two qs
    m<-mu* Q+ omega*Q_o
    
    # softmax distribution as in Christakou et al. (2013)
    p<-exp(m)/sum(exp(m))

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
      
      # update the counter 
      count2<-count2+1
    }
    # assign values to the dataset
    Data[t, Qindex]<-Q
    Data[t, Pindex]<-p
    
    # now update according to the category that is shown at the end of the trial
    delta_o<- x[t,]-Q_o
    Q_o<-Q_o+alpha_o*delta_o
    
    Data[t, delta_o_index]<-delta_o
    Data[t, Q_oindex]<-Q_o
    
  }
  # take only the prob for the high prior scenes
  Probstrong<-getProbStrongWeak(Data, setup)
  # compute negative log-likelihood as the negative sum of log prob relative to the choice
  NegLL<--sum(log(Probstrong), na.rm=T)
  
  if (print ==1){
    return(NegLL)
  }else if ( print==2){
    return (list("Negative LogLikel"=NegLL, "Q1"= Data$Q1,"Q2"= Data$Q2,"Q3"= Data$Q3,"Q1_o"= Data$Q1_o,"Q2_o"= Data$Q2_o,"Q3_o"= Data$Q3_o,
                 "Delta"= Data$Delta,"Delta_o"= Data$Delta_o,"P1"=Data$P1,"P2"= Data$P2, "P3"=Data$P3 ))
  } else if(print==3){
    return(Data)}
}

