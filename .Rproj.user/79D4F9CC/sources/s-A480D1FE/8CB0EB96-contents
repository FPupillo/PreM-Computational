lik_RescorlaWagner_obsALL<-function (Data,alpha, beta,print,  initialQ){
  # This function computes the likelihood of the participants'
  # choices conditional on the Rescorla Wagner model 
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
  
  
  
  # convert the resp flower into numeric variable
  levels(Data$respFlower)
  
  #1 "blue_flower"  2  "green_flower" 3 "red_flower"  4  "yellow_flower"
  Data$FlowNum<-as.numeric((Data$respFlower))
  
  Data$corrFlower<-as.numeric((Data$corrFlower))
  
  
  # Initialize variables: Qs, the expected values
  Data$Q1<-NA; Data$Q2<-NA; Data$Q3<-NA ; Data$Q4<-NA 
  
  # Ps (probabilities for each flower's choice)
  Data$P1<-NA; Data$P2<-NA; Data$P3<-NA ; Data$P4<-NA
  
  # probability for the choice that participants' made on a trial
  Data$Prob<-NA
  
  # Delta, prediction error
  Data$Delta<-NA
  
  # uncertainty as the 1/variance of the probability
  Data$uncertainty<-NA
  # uncertainty as the -log(sum prob * log prob)
  Data$uncertainty2<-NA
  # change point probability
  Data$CPP<-NA
  
  # index variables for Q, P, and Delta
  Qindex<-c("Q1", "Q2", "Q3", "Q4")
  Pindex<-c("P1", "P2", "P3", "P4") 
  Deltaindex<-c("Delta1", "Delta2", "Delta3", "Delta4")
  
  # Counter for indicating which butterfly has to be updated
  count<-rep(0, 2)
  
  # convert butterfly as numeric
  Data$butterfly<-as.character(Data$butterfly)
  for (t in 1:nrow(Data)){
    if(Data$butterfly[t]=="white_butterfly"){
      Data$butterfly[t]<-1
    }else{Data$butterfly[t]<-2}
  }
  Data$butterfly<-as.numeric(Data$butterfly)
  
  # initialise choice probability and counter for the choiceprobability
  prob<-NA
  count2<-1
  
  
  # loop over trials
  for (t in 1: max(Data$trialNum)){
    
    # The following loop retrieves the Q values of the butterfly that corresponds to the current trial (time t).
    if (count[Data$butterfly[t]]==0){
      Q<-c(initialQ, initialQ, initialQ, initialQ) # if it is the first time that butterfly is shown, the Qs are at their initial value
      count[Data$butterfly[t]]<-count[Data$butterfly[t]]+1 # update the counter
    } else{
      Q<-Data[Data$butterfly==Data$butterfly[t],][count[Data$butterfly[t]],Qindex] # if it is not the first time that butterfly is shown, retrieve the Qs of the last trial of that butterfly
      count[Data$butterfly[t]]<-count[Data$butterfly[t]]+1 # update the counter
    }
    
    # update choice probabilities using the softmax distribution
    p<-softmax(Q, beta)
    
    # compute Q, delta, and choice probability for actual choice, only if a choice is computed
    if (Data$response[t]!=0 & !is.na(Data$FlowNum[t]) ){
      
      # probability only for the response made by participant
      prob[count2]<-unlist(p[Data$FlowNum[t]])
      
      # assign it to the dataset
      Data$Prob[t]<- prob[count2]
      
    }
    
    # get the observation as 1 if that flower is present, and 0 if it is not
    if (Data$corrFlower[t]==1){
      obs<-c(1,0,0,0)
    } else if (Data$corrFlower[t]==2){
      obs<-c(0,1,0,0)
    } else if (Data$corrFlower[t]==3){
      obs<-c(0,0,1,0)
    }else if (Data$corrFlower[t]==4){
      obs<-c(0,0,0,1)
    }
        # prediction error
        delta <- obs - Q
        
        # assign it to the dataset
        Data[t, Deltaindex]<-delta
        
        # update all the Qs
        Q<-Q+ alpha *delta
        
        # update the counter 
        count2<-count2+1
  
  # assign values to the dataset
  Data[t, Qindex]<-Q
  Data[t, Pindex]<-p
  
  # compute uncertainty as the 1/variance of the probability
  uncertainty<-1/(var(unlist(Data[t,Pindex]))+1)
  
  # uncertainty as the -log(sum prob * log prob)
  uncertainty2<- -sum(  unlist(Data[t,Pindex]) *log(unlist(Data[t,Pindex])))
  
  # change point probability
  # probability of that observations
  probobs<-Data[t, paste("P", Data$corrFlower[t], sep="")]
  
  if (count[Data$butterfly[t]]==1 |count[-Data$butterfly[t]]==0 ){ # while one of the two butterlfy has not been shown
    
    Data$uncertainty[t]<-uncertainty
    Data$uncertainty2[t]<-uncertainty2
    Data$CPP[t]<-probobs
    # uncertainty as the -log(sum prob * log prob)
    
  } else{
    # retrieve uncertainty on the last trial of the other butterfly
    unctmin1<-Data[Data$butterfly!=Data$butterfly[t],][count[-Data$butterfly[t]], "uncertainty"]
    
    unc2tmin1<-Data[Data$butterfly!=Data$butterfly[t],][count[-Data$butterfly[t]], "uncertainty2"]
    
    # retrieve probability of the observation on the previous trial for the same butterfly
    probobstminus1<-Data[Data$butterfly==Data$butterfly[t],][count[Data$butterfly[t]]-1,paste("P", Data$corrFlower[t], sep="") ]
    
    Data$uncertainty[t]<-mean(uncertainty,unctmin1)
    Data$uncertainty2[t]<-mean(uncertainty2,unctmin1)
    Data$CPP[t]<-probobs*probobstminus1
    
    
  }
}
# we could take the probability only for the congruent trials, but for now we are taking all the probabilities

NegLL<--sum(log(Data$Prob), na.rm=T)

if (print ==1){
  return(NegLL)
}else if ( print==2){
  return (list("Negative LogLikel"=NegLL, "Q1"= Data$Q1,"Q2"= Data$Q2,"Q3"= Data$Q3,"Q4"= Data$Q4,
               "Delta"= Data$Delta,"P1"=Data$P1,"P2"= Data$P2, "P3"=Data$P3 , "P4"=Data$P4))
} else if(print==3){
  return(Data)}
}

