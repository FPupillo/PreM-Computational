#------------------------------------------------------------------------------------------------#
# function that returns a vector called "x" with the feedback dummy coded
# it is used in the Rescorla Wagner formulas and optimal Bayesian models
#------------------------------------------------------------------------------------------------#

getfeedb<-function(Data, initialQ){
  
  # initialize the matrix
  x<-matrix(NA, nrow=nrow(Data), ncol=ncol(initialQ))
  
  if (ncol(initialQ)==2){ # if there are only two choices
    for (n in 1:length(Data$response)){
      
      if (Data$response[n]!=0){
        
        if (Data$acc[n]==1){
        
        if (Data$response[n]==1){
          
          x[n,]<-c(1,0)
          
        } else if ((Data$response[n]==2)){
          
          x[n,]<-c(0,1)
        }
      } else {x[n,]<-c(0,0)
      }
        # if there is no response the outcome is 0
      } else{ x[n,]<-c(0,0)}
    }
    
    
  }else if (ncol(initialQ)==3){ # if there are three object categories
    
    for (n in 1:length(Data$response)){
      
      if (Data$response[n]!=0){
        
        if (Data$acc[n]==1){
        
        if (Data$response[n]==1){
          
          x[n,]<-c(1,0,0)
          
        } else if ((Data$response[n]==2)){
          
          x[n,]<-c(0,1,0)
          
        } else if (Data$response[n]==3){
          
          x[n,]<-c(0,0,1)}
        
        } else {x[n,]<-c(0,0, 0)}
        
        # if there is no response, outcome is 0
    } else{ x[n,]<-c(0,0, 0)}
    }
  }
  
  return(x)
}
