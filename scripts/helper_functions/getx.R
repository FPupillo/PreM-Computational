# function that returns a matrix calle "x" with the response dummy coded
# it is used in the rescorla wagner formulas to obtain the feedback (observation)

getx<-function(Data, initialQ){
  
# initialize the matrix
x<-matrix(NA, nrow=nrow(Data), ncol=ncol(initialQ))

if (ncol(initialQ)==2){ # if there are only two choices
  for (n in 1:length(Data$object_cat)){
    if (Data$object_cat[n]==1){
      x[n,]<-c(1,0)
    } else if ((Data$object_cat[n]==2)){
      x[n,]<-c(0,1)
    }
  }
  
}else if (ncol(initialQ)==3){ # if there are three object categories
  
  for (n in 1:length(Data$object_cat)){
    if (Data$object_cat[n]==1){
      x[n,]<-c(1,0,0)
    } else if ((Data$object_cat[n]==2)){
      x[n,]<-c(0,1,0)
    } else if (Data$object_cat[n]==3){
      x[n,]<-c(0,0,1)}
  }
}

return(x)
}
