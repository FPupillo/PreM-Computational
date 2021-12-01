#library(pracma)

fit_all<-function(data,alphaBound, betaBound,
                  initialQ){
  
  # ---------------------------------------------------------------------------#
  # This function fits all the model to a dataset and return the best one and 
  # the BIC 
  # 
  # Input    
  #   Data: a long dataset where each row represents a trial of simulated data
  #   alphaBound<- a two-element vector with upper and lower boundaries for the
  #               alpha parameter     
  #   betaBound<- a two-element vector with upper and lower boundaries 
  #               for the beta parameter 
  #   initialQ <- values at which the Qs are initiated 
  #   initialCK<- initial value of the choice kernel
  # 
  #  "iBEST": avector indicating which model is the best
  # ---------------------------------------------------------------------------#
  
  # initialize the Bic vector
  BIC<-vector()
  
  print("Fitting dLR_Instr")
  BIC[1]<-fit_dLR_Instr(data, betaBound, initialQ)[[3]]
  print("Fitting fLR_Instr model")
  BIC[2]<-fit_fLR_Instr(data,alphaBound, betaBound, initialQ )[[3]]
  print("Fitting fLR_Eval model")
  BIC[3]<-fit_fLR_Eval(data,alphaBound, betaBound, initialQ )[[3]]

  # which model had the best fit?
  iBest<-as.numeric(BIC==min(BIC))
  
  # what was the best Bic?
  return (iBest)

}
