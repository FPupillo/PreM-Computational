getResp<-function(data, feedback){
  # Function used to take the responses made by participants and turn them into a 
  # three-element binary array, used by the model functions (delta rule)
  # 
  # Input
  #   data: a long dataset where each row represents a trial, and data$response represents the responses made by participants
  #   feedback: 1 = take the feedback into account. if accuracy is equal to 1, update with 1 the object category
  #            2 = take only the response into account and not the feedback. update the category of the response
  #            3 = take only the category participants are presented with
  # Output:
  #   a matrix where each row represents a trial and each column represents the object category (1,2,3)
  #
  # -------------------------
  
  # initialize matrix
  T<- length(data[,1])
  x<-matrix(NA, nrow = T, ncol = 3)
  
  
  if (feedback==1){ # taking into account feedback
    for (n in 1:T){
      if (data$acc[n]==1){
        if (data$object_cat[n]==1){
          x[n,]<-c(1,0,0)
        } else if ((data$object_cat[n]==2)){
          x[n,]<-c(0,1,0)
        } else if (data$object_cat[n]==3){
          x[n,]<-c(0,0,1)}
      } else{
        x[n,]<-c(0,0,0)
      }
    }
  }else if (feedback==2){
    for (n in 1:T){
      if (data$response[n]==1){
        x[n,]<-c(1,0,0)
      } else if ((data$response[n]==2)){
        x[n,]<-c(0,1,0)
      } else if (data$response[n]==3){
        x[n,]<-c(0,0,1)
      } else if (data$response[n]==0){
        x[n,]<-c(0,0,0)}
    } 
  } else if (feedback==3){
    for (n in 1:T){
      if (data$object_cat[n]==1){
        x[n,]<-c(1,0,0)
      } else if ((data$object_cat[n]==2)){
        x[n,]<-c(0,1,0)
      } else if (data$object_cat[n]==3){
        x[n,]<-c(0,0,1)
      } else{
        x[n,]<-c(0,0,0)
      }
    }    
  }
  
  return(x)
}
  
  