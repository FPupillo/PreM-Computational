searchGlobal<-function(data, alphaBound, betaBound, 
                       startPoints, initialQ, fittingfunction , model){
  #----------------------------------------------------------------------------#
  # This function tries to find the global maximum of the log likelihood
  # 
  # Input
  #    Data: a long dataset where each row represents a trial.
  #    alphaBound<- a two-element vector with upper and lower boundaries for 
  #                 the alpha parameter     
  #    betaBound<- a two-element vector with upper and lower boundaries for the 
  #                 beta parameter 
  #    startPoints<- number of starting points: it determines how many times 
  #                 the optimization algorithm is initiated 
  #                 with different starting points
  #    initialQ<-initial Q values
  #    fittingfunction<-function used to fit the model 
  #    model<- kind of model
  #
  # Output
  #   A list object with: "alpha" = learning rate
  #                       "beta"= inverse temperature
  #                       "logLikel"= log-likelihood for the parameter estimated 
  #                       "BIC"= BIC for the parameter estimated
  # 
  #----------------------------------------------------------------------------#
  
  # create progress bar
  pb<-txtProgressBar(min=0, max=startPoints)
  # create matrix to store value
  
  if (model == "fLR_Instr" | model == "fLR_Eval"){
    
    Matrix<-matrix(NA, nrow = startPoints, ncol=4)
    
    colnames(Matrix)<-c("alpha", "beta","logLikel", "BIC")
      
      for (i in 1:startPoints){
        fit<-fittingfunction(data = data,alphaBound =  alphaBound, 
                             betaBound = betaBound, initialQ = initialQ)
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
    
  }  else if (model == "dLR_Instr" ){
    
    Matrix<-matrix(NA, nrow = startPoints, ncol=3)
    
    colnames(Matrix)<-c( "beta",  "logLikel", "BIC")
    
    for (i in 1:startPoints){
      fit<-fittingfunction(data,betaBound, initialQ )
      Matrix[i,]<-c(fit[[1]], fit[[2]], fit[[3]] )
      
      #progress bar
      setTxtProgressBar(pb, i) 
      
    }
    # which is the maximum for the best fitting parameters?
    max<-which(Matrix[,3]==max(Matrix[,3]))
    beta<-Matrix[max,1][1]
    loglik<-Matrix[max,2][1]
    BIC<-Matrix[max, 3][1]
    
    print("Finished parameter estimation")
    
    return(list("beta"=as.numeric(beta),  
                "logLikel"=as.numeric(loglik), "BIC"=as.numeric(BIC)) )
    
  } else{
    print("Error = Sorry, I could not find the model. Please make sure the name of the model is correct")
  }
}
