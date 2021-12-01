

update<-function(r, Q, alpha){
  # This function returns the updated value q and delta according to 
  # Rescorla Wagner model
  #
  # Input
  #   r: reinforcement (feedback): 1 if correct, 0 if incorrect
  #   Q: value
  #   alpha: alpha parameter (learning rate)
  #
  # Output:
  #   updated Q and Delta (prediction error)
  # -------------
  delta = r - Q;
  Q = Q + alpha * delta
  return(list("Q"= Q, "delta"= delta))  
}
