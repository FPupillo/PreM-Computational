#library(pracma)

fit_RescorlaWagner_obsALLdualLR<-function(data,alphaposBound,alphanegBound, betaBound, initialQ){
  
  #------------------------------------------------------------------------------------------------#
  # This function finds the parameters that 
  # minimize the negative log-likelihood
  #
  # Input    
  #    Data: a long dataset where each row represents a trial. 
  #    alphaposBound<- a two-element vector with upper and lower boundaries for the alpha parameter
  #                    for positive learning rate (correct answer, positive PE)
  #    alphanegBound<- a two-element vector with upper and lower boundaries for the alpha parameter
  #                    for negative learning rate (incorrect answer, negative PE)
  #    betaBound<- a two-element vector with upper and lower boundaries for the beta parameter 
  #    initialQ<- inital Q values
  #
  # Output:
  #   A list with: 
  #   [[1]] "alphabetaPAR" : alpha [1], beta [2]parameters that minimize the negative log-likelihood
  #   [[2]] "loglikel": log-likelihood for the model with the parameters of best fit
  #   [[3]] "BIC" : Bayesian Information Criterion for the model with the parameters of best fit
  #------------------------------------------------------------------------------------------------#
  
  # rexp generates random numbers from the exponential distributon with mean 1
  X0<-c(runif(1),runif(1), rexp(1,1))  
  LB<-c(alphaposBound[1], alphanegBound[1], betaBound[1]) # lower boundary
  UB<-c(alphaposBound[2], alphanegBound[2],betaBound[2]) 

  # this function is similar to the MATLAB "handle" function
  obfunc<-function(x) lik_RescorlaWagner_obsALLdualLR(Data = data,alpha = x[1],
                                                      alpha_o = x[2], beta = x[3],
                                                      print = 1,initialQ= initialQ) 
  
  # Find best-fitting parameters
  NegLL<-optim(X0, obfunc, method = "L-BFGS-B",lower = LB, upper=UB) 
  
  # get log-likelihood
  LL<--NegLL[[2]] # log likelihood
  
  # compute BIC
  BIC <- BICcompute(length(X0), length(data$scene_cat), NegLL[[2]])
  
  # Prepare results for output
  data <- list(NegLL[[1]], LL, BIC)
  names(data)<-c("alphabetaPAR", "logLikel", "BIC" )
  
  return(data)
}
