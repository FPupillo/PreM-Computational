#library(pracma)

fit_RescorlaWagner_obsALLstick<-function(
  data,alphaBound, betaBound, omegaBound, initialQ, initialU)
  {
  
  #------------------------------------------------------------------------------------------------#
  # This function finds the parameters that 
  # minimize the negative log-likelihood
  #
  # Input    
  #    Data: a long dataset where each row represents a trial. 
  #    alphaBound<- a two-element vector with upper and lower boundaries for the alpha parameter     
  #    betaBound<- a two-element vector with upper and lower boundaries for the beta parameter 
  #    initialQ<- inital Q values
  #    initialU<-inital U values
  #    omegaBound<- upper and lower boundaries for the omega parameter
  #    setup<- premup flat, priors, pilot, three
  # Output:
  #   A list with: 
  #   [[1]] "alphabetaPAR" : alpha [1], beta [2]parameters that minimize the negative log-likelihood
  #   [[2]] "loglikel": log-likelihood for the model with the parameters of best fit
  #   [[3]] "BIC" : Bayesian Information Criterion for the model with the parameters of best fit
  # -----------------------------------------------------------------------------------------------#
  
  # rexp generates random numbers from the exponential distributon with mean 1
  X0<-c( runif(1), rexp(1,1), rnorm(1))  
  LB<-c(alphaBound[1], betaBound[1],   omegaBound[1]) # lower boundary
  UB<-c(alphaBound[2],   betaBound[2] ,omegaBound[2]) 

  # this function is similar to the MATLAB "handle" function
  obfunc<-function(x) lik_RescorlaWagner_obsALLstick(data, x[1], x[2], x[3],1, initialQ, initialU
                                                     ) 
  
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
