# choose multinom function

chooseMultinom <- function(p){
  # Function to simulate choice made by particiapnt, using 
  #   multinomial function in R (generating random vectors from the multinomial distribution)
  # 
  # Input
  #  p : a vector with the probabilities
  #
  # Output
  #   a : a vector indicating the choice
  # ------------------
  a<-rmultinom(1,1, p)
  
  # transform the matrix into a vector
  a<-a[,1]
  
  # make it a number between 1 and 3
  a<-which(a==1)
  return(a)
}

