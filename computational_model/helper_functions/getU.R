# function that returns a vector called "U" with the response dummy coded
# it is used in the rescorla wagner formulas optimal bayesian models

getU<-function(Data, initialQ,t){
  
  if (ncol(initialQ)==2){
    if (Data$response[t]==1){
      U<-c(1,0)
    } else if (Data$response[t]==2){
      U<-c(0,1)
    } else {# no response
      U<-c(0,0)
    }
    
  }else if(ncol(initialQ)==3){
    if (Data$response[t]==1){
      U<-c(1,0,0)
    } else if (Data$response[t]==2){
      U<-c(0,1,0)
    } else if (Data$response[t]==3){
      U<-c(0,0,1)
    } else { # no response
      U<-c(0,0,0)
      
    }
  }

return(U)
}