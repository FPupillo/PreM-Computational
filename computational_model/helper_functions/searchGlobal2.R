searchGlobal<-function(data, alphaBound, betaBound, startPoints, initialQ){
  # This function tries to find the global maximum of the log likelihood
  # 
  # Input
  #    Data: a long dataset where each row represents a trial.
  #    alphaBound<- a two-element vector with upper and lower boundaries for the alpha parameter     
  #    betaBound<- a two-element vector with upper and lower boundaries for the beta parameter 
  #    startPoints<- number of starting points: it determines how many times the optimization algorithm is initiated 
  #                 with different starting points
  #    initialQ<-initial Q values
  #
  # Output
  #   A list object with: "alpha" = learning rate
  #                       "beta"= inverse temperature
  #                       "logLikel"= log-likelihood for the parameter estimated 
  #                       "BIC"= BIC for the parameter estimated
  # 
  # ------------------------
  
  # create progress bar
  pb<-txtProgressBar(min=0, max=startPoints)
  # create matrix to store value
  
  Matrix<-matrix(NA, nrow = startPoints, ncol=4)
  colnames(Matrix)<-c("alpha", "beta", "logLikel", "BIC")
  # You should definitely parameterize this. I.e., add a variable in 06.Parameter_estimation which allows us to change these values.
  # For debugging purposes, I set it to 1
  for (i in 1:startPoints){
    fit<-fit_RescorlaWagner_feedb(data,alphaBound,betaBound , initialQ)
    Matrix[i,]<-c(fit[[1]][1], fit[[1]][2],fit[[2]], fit[[3]] )
    
    #progress bar
    setTxtProgressBar(pb, i) 
    
  }
  # which is the maximum for the best fitting parameters?
  max<-which(Matrix[,3]==max(Matrix[,3]))
  alpha<-Matrix[max,1][1]
  beta<-Matrix[max,2][1]
  loglik<-Matrix[max,3][1]
  BIC<-Matrix[max, 4][1]
  
  print("Finished parameter estimation")
  
  return(list("alpha"=as.numeric(alpha),"beta"=as.numeric(beta), "logLikel"=as.numeric(loglik), "BIC"=as.numeric(BIC) ))
  
}

searchGlobalQ<-function(data, alphaBound, betaBound, startPoints){
  # This function tries to find the global maximum of the log likelihood, considering RW model and Q
  # 
  # Input
  #    Data: a long dataset where each row represents a trial.
  #    alphaBound<- a two-element vector with upper and lower boundaries for the alpha parameter     
  #    betaBound<- a two-element vector with upper and lower boundaries for the beta parameter 
  #    startPoints<- number of starting points: it determines how many times the optimization algorithm is initiated 
  #                 with different starting points
  #    initialQ<-initial Q values
  #
  # Output
  #   A list object with: "alpha" = learning rate
  #                       "beta"= inverse temperature
  #                       "logLikel"= log-likelihood for the parameter estimated 
  #                       "BIC"= BIC for the parameter estimated
  # 
  # ------------------------
  
  # create progress bar
  pb<-txtProgressBar(min=0, max=startPoints)
  # create matrix to store value
  
  Matrix<-matrix(NA, nrow = startPoints, ncol=5)
  colnames(Matrix)<-c("alpha", "beta", "Q", "logLikel", "BIC")
  # You should definitely parameterize this. I.e., add a variable in 06.Parameter_estimation which allows us to change these values.
  # For debugging purposes, I set it to 1
  for (i in 1:startPoints){
    fit<-fit_RescorlaWagner_feedbAndQ(data,alphaBound,betaBound )
    Matrix[i,]<-c(fit[[1]][1], fit[[1]][2],fit[[1]][3], fit[[2]], fit[[3]] )
    
    #progress bar
    setTxtProgressBar(pb, i) 
    
  }
  # which is the maximum for the best fitting parameters?
  max<-which(Matrix[,3]==max(Matrix[,3]))
  alpha<-Matrix[max,1][1]
  beta<-Matrix[max,2][1]
  Q<-Matrix[max,3][1]
  loglik<-Matrix[max,4][1]
  BIC<-Matrix[max, 5][1]
  
  print("Finished parameter estimation")
  
  return(list("alpha"=as.numeric(alpha),"beta"=as.numeric(beta), "Q" = as.numeric(Q), "logLikel"=as.numeric(loglik), "BIC"=as.numeric(BIC)) )
  
}

searchGlobalCK<-function(data, alphaBound, betaBound, startPoints, initialCK){
  # This function tries to find the global maximum of the log likelihood
  # 
  # Input
  #    Data: a long dataset where each row represents a trial.
  #    alphaBound<- a two-element vector with upper and lower boundaries for the alpha parameter     
  #    betaBound<- a two-element vector with upper and lower boundaries for the beta parameter 
  #    startPoints<- number of starting points: it determines how many times the optimization algorithm is initiated 
  #                 with different starting points
  #    initialCK<-initial CK values
  #
  # Output
  #   A list object with: "alpha" = learning rate
  #                       "beta"= inverse temperature
  #                       "logLikel"= log-likelihood for the parameter estimated 
  #                       "BIC"= BIC for the parameter estimated
  # 
  # ------------------------
  
  # create progress bar
  pb<-txtProgressBar(min=0, max=startPoints)
  # create matrix to store value
  
  Matrix<-matrix(NA, nrow = startPoints, ncol=4)
  colnames(Matrix)<-c("alpha", "beta", "logLikel", "BIC")
  # You should definitely parameterize this. I.e., add a variable in 06.Parameter_estimation which allows us to change these values.
  # For debugging purposes, I set it to 1
  for (i in 1:startPoints){
    fit<-fit_ChoiceKernel(data,alphaBound,betaBound , initialCK)
    Matrix[i,]<-c(fit[[1]][1], fit[[1]][2],fit[[2]], fit[[3]] )
    
    #progress bar
    setTxtProgressBar(pb, i) 
    
  }
  # which is the maximum for the best fitting parameters?
  max<-which(Matrix[,3]==max(Matrix[,3]))
  alpha<-Matrix[max,1][1]
  beta<-Matrix[max,2][1]
  loglik<-Matrix[max,3][1]
  BIC<-Matrix[max, 4][1]
  
  print("Finished parameter estimation")
  
  return(list("alpha"=as.numeric(alpha),"beta"=as.numeric(beta), "logLikel"=as.numeric(loglik), "BIC"=as.numeric(BIC) ))
  
}

searchGlobalObs<-function(data, alphaBound, betaBound, startPoints, initialQ){
  # This function tries to find the global maximum of the log likelihood
  # 
  # Input
  #    Data: a long dataset where each row represents a trial.
  #    alphaBound<- a two-element vector with upper and lower boundaries for the alpha parameter     
  #    betaBound<- a two-element vector with upper and lower boundaries for the beta parameter 
  #    startPoints<- number of starting points: it determines how many times the optimization algorithm is initiated 
  #                 with different starting points
  #    initialQ<-initial Q values
  #
  # Output
  #   A list object with: "alpha" = learning rate
  #                       "beta"= inverse temperature
  #                       "logLikel"= log-likelihood for the parameter estimated 
  #                       "BIC"= BIC for the parameter estimated
  # 
  # ------------------------
  
  # create progress bar
  pb<-txtProgressBar(min=0, max=startPoints)
  # create matrix to store value
  
  Matrix<-matrix(NA, nrow = startPoints, ncol=4)
  colnames(Matrix)<-c("alpha", "beta", "logLikel", "BIC")
  # You should definitely parameterize this. I.e., add a variable in 06.Parameter_estimation which allows us to change these values.
  # For debugging purposes, I set it to 1
  for (i in 1:startPoints){
    fit<-fit_RescorlaWagner_obs(data,alphaBound,betaBound , initialQ)
    Matrix[i,]<-c(fit[[1]][1], fit[[1]][2],fit[[2]], fit[[3]] )
    
    #progress bar
    setTxtProgressBar(pb, i) 
    
  }
  # which is the maximum for the best fitting parameters?
  max<-which(Matrix[,3]==max(Matrix[,3]))
  alpha<-Matrix[max,1][1]
  beta<-Matrix[max,2][1]
  loglik<-Matrix[max,3][1]
  BIC<-Matrix[max, 4][1]
  
  print("Finished parameter estimation")
  
  return(list("alpha"=as.numeric(alpha),"beta"=as.numeric(beta), "logLikel"=as.numeric(loglik), "BIC"=as.numeric(BIC) ))
  
}

searchGlobalObsCA<-function(data, alphaBound, betaBound, startPoints, initialQ){
  # This function tries to find the global maximum of the log likelihood
  # 
  # Input
  #    Data: a long dataset where each row represents a trial.
  #    alphaBound<- a two-element vector with upper and lower boundaries for the alpha parameter     
  #    betaBound<- a two-element vector with upper and lower boundaries for the beta parameter 
  #    startPoints<- number of starting points: it determines how many times the optimization algorithm is initiated 
  #                 with different starting points
  #    initialQ<-initial Q values
  #
  # Output
  #   A list object with: "alpha" = learning rate
  #                       "beta"= inverse temperature
  #                       "logLikel"= log-likelihood for the parameter estimated 
  #                       "BIC"= BIC for the parameter estimated
  # 
  # ------------------------
  
  # create progress bar
  pb<-txtProgressBar(min=0, max=startPoints)
  # create matrix to store value
  
  Matrix<-matrix(NA, nrow = startPoints, ncol=4)
  colnames(Matrix)<-c("alpha", "beta", "logLikel", "BIC")
  # You should definitely parameterize this. I.e., add a variable in 06.Parameter_estimation which allows us to change these values.
  # For debugging purposes, I set it to 1
  for (i in 1:startPoints){
    fit<-fit_RescorlaWagner_obsCA(data,alphaBound,betaBound , initialQ)
    Matrix[i,]<-c(fit[[1]][1], fit[[1]][2],fit[[2]], fit[[3]] )
    
    #progress bar
    setTxtProgressBar(pb, i) 
    
  }
  # which is the maximum for the best fitting parameters?
  max<-which(Matrix[,3]==max(Matrix[,3]))
  alpha<-Matrix[max,1][1]
  beta<-Matrix[max,2][1]
  loglik<-Matrix[max,3][1]
  BIC<-Matrix[max, 4][1]
  
  print("Finished parameter estimation")
  
  return(list("alpha"=as.numeric(alpha),"beta"=as.numeric(beta), "logLikel"=as.numeric(loglik), "BIC"=as.numeric(BIC) ))
  
}

searchGlobalfeedbCK<-function(data,alphaBound, betaBound,alpha_cBound, beta_cBound, startPoints, initialQ, initialCK){
  # This function tries to find the global maximum of the log likelihood
  # 
  # Input
  #    Data: a long dataset where each row represents a trial. 
  #    alphaBound<- a two-element vector with upper and lower boundaries for the alpha parameter  
  #    alpha_cBound<- a two-element vector with upper and lower boundaries for the alpha parameter     
  #    betaBound<- a two-element vector with upper and lower boundaries for the beta parameter 
  #    beta_cBound<- a two-element vector with upper and lower boundaries for the beta parameter 
  #    initialQ<- inital Q values
  #    initialCK<- inital Q values
  #
  # Output
  #   A list object with: "alpha" = learning rate
  #                       "beta"= inverse temperature
  #                       "logLikel"= log-likelihood for the parameter estimated 
  #                       "BIC"= BIC for the parameter estimated
  # 
  # ------------------------
  
  # create progress bar
  pb<-txtProgressBar(min=0, max=startPoints)
  # create matrix to store value
  
  Matrix<-matrix(NA, nrow = startPoints, ncol=6)
  colnames(Matrix)<-c("alpha", "beta", "alpha_c", "beta_c" ,"logLikel", "BIC")
  # You should definitely parameterize this. I.e., add a variable in 06.Parameter_estimation which allows us to change these values.
  # For debugging purposes, I set it to 1
  for (i in 1:startPoints){
    fit<-fit_RescorlaWagner_feedbCK(data,alphaBound,  betaBound, alpha_cBound, beta_cBound, initialQ, initialCK)
    Matrix[i,]<-c(fit[[1]][1], fit[[1]][2],fit[[1]][3], fit[[1]][4], fit[[2]], fit[[3]] )
    
    #progress bar
    setTxtProgressBar(pb, i) 
    
  }
  # which is the maximum for the best fitting parameters?
  max<-which(Matrix[,5]==max(Matrix[,5]))
  alpha<-Matrix[max,1][1]
  beta<-Matrix[max,2][1]
  alpha_c<-Matrix[max,3][1]
  beta_c<-Matrix[max,4][1]
  loglik<-Matrix[max,5][1]
  BIC<-Matrix[max, 6][1]
  
  print("Finished parameter estimation")
  
  return(list("alpha"=as.numeric(alpha),"beta"=as.numeric(beta),
              "alpha_c" = as.numeric(alpha_c), "beta_c"<-as.numeric(beta_c),
              "logLikel"=as.numeric(loglik), "BIC"=as.numeric(BIC) ))
  
}

searchGlobalfeedbAndObs<-function(data,alphaBound, betaBound,alpha_oBound, beta_oBound, startPoints, initialQ, initialQ_o){
  # This function tries to find the global maximum of the log likelihood
  # 
  # Input
  #    Data: a long dataset where each row represents a trial. 
  #    alphaBound<- a two-element vector with upper and lower boundaries for the alpha parameter (model updated with feedback)
  #    betaBound<- a two-element vector with upper and lower boundaries for the beta parameter (model updated with the feedback)
  #    alpha_oBound<- a two-element vector with upper and lower boundaries for the alpha parameter    (model updated with the category) 
  #    beta_oBound<- a two-element vector with upper and lower boundaries for the beta parameter (model updated with the category)
  #    initialQ<- inital Q values
  #    initialQ_o<- inital Q values for the 
  #
  # Output
  #   A list object with: "alpha" = learning rate for RW with feedb
  #                       "beta"= inverse temperature for RW with category
  #                       "alpha_o" = learning rate for RW with feedb
  #                       "beta_o"= inverse temperature for RW with category
  #                       "logLikel"= log-likelihood for the parameter estimated 
  #                       "BIC"= BIC for the parameter estimated
  # 
  # ------------------------
  
  # create progress bar
  pb<-txtProgressBar(min=0, max=startPoints)
  # create matrix to store value
  
  Matrix<-matrix(NA, nrow = startPoints, ncol=6)
  colnames(Matrix)<-c("alpha", "beta", "alpha_o", "beta_o" ,"logLikel", "BIC")
  # You should definitely parameterize this. I.e., add a variable in 06.Parameter_estimation which allows us to change these values.
  # For debugging purposes, I set it to 1
  for (i in 1:startPoints){
    fit<-fit_RescorlaWagner_feedbAndObs(data,alphaBound,  betaBound, alpha_oBound, beta_oBound, initialQ, initialQ_o)
    Matrix[i,]<-c(fit[[1]][1], fit[[1]][2],fit[[1]][3], fit[[1]][4], fit[[2]], fit[[3]] )
    
    #progress bar
    setTxtProgressBar(pb, i) 
    
  }
  # which is the maximum for the best fitting parameters?
  max<-which(Matrix[,5]==max(Matrix[,5]))
  
  alpha<-Matrix[max,1][1]
  beta<-Matrix[max,2][1]
  alpha_o<-Matrix[max,3][1]
  beta_o<-Matrix[max,4][1]
  loglik<-Matrix[max,5][1]
  BIC<-Matrix[max, 6][1]
  
  print("Finished parameter estimation")
  
  return(list("alpha"=as.numeric(alpha),"beta"=as.numeric(beta),
              "alpha_o" = as.numeric(alpha_o), "beta_o"<-as.numeric(beta_o),
              "logLikel"=as.numeric(loglik), "BIC"=as.numeric(BIC) ))
  
}

searchGlobalfeedbALL<-function(data,alphaBound, betaBound, startPoints, initialQ){
  # This function tries to find the global maximum of the log likelihood
  # 
  # Input
  #    Data: a long dataset where each row represents a trial. 
  #    alphaBound<- a two-element vector with upper and lower boundaries for the alpha parameter (model updated with feedback)
  #    betaBound<- a two-element vector with upper and lower boundaries for the beta parameter (model updated with the feedback)
  #    initialQ<- inital Q values
  #
  # Output
  #   A list object with: "alpha" = learning rate for RW with feedb
  #                       "beta"= inverse temperature for RW with category
  #                       "logLikel"= log-likelihood for the parameter estimated 
  #                       "BIC"= BIC for the parameter estimated
  # 
  # ------------------------
  
  # create progress bar
  pb<-txtProgressBar(min=0, max=startPoints)
  # create matrix to store value
  
  Matrix<-matrix(NA, nrow = startPoints, ncol=4)
  colnames(Matrix)<-c("alpha", "beta","logLikel", "BIC")
  # You should definitely parameterize this. I.e., add a variable in 06.Parameter_estimation which allows us to change these values.
  # For debugging purposes, I set it to 1
  for (i in 1:startPoints){
    fit<-fit_RescorlaWagner_feedbALL(data,alphaBound,  betaBound, initialQ)
    Matrix[i,]<-c(fit[[1]][1], fit[[1]][2],fit[[2]], fit[[3]] )
    #progress bar
    setTxtProgressBar(pb, i) 
    
  }
  # which is the maximum for the best fitting parameters?
  max<-which(Matrix[,3]==max(Matrix[,3]))
  alpha<-Matrix[max,1][1]
  beta<-Matrix[max,2][1]
  loglik<-Matrix[max,3][1]
  BIC<-Matrix[max, 4][1]
  
  print("Finished parameter estimation")
  
  return(list("alpha"=as.numeric(alpha),"beta"=as.numeric(beta),
              "logLikel"=as.numeric(loglik), "BIC"=as.numeric(BIC) ))
  
}
