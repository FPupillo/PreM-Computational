#------------------------------------------------------------------------------------------------#
# function that returns a vector called "x" with the response dummy coded
# it is used in the Rescorla Wagner formulas optimal Bayesian models
#------------------------------------------------------------------------------------------------#

getobs<-function(Data, t, xindex){

  if (ncol(initialQ)==2){
    if (Data$object_cat[t]==1){
      Data[t,xindex]<-c(1,0)
    } else if (Data$object_cat[t]==2){
      Data[t,xindex]<-c(0,1)
    }
  }else if(ncol(initialQ)==3){
    if (Data$object_cat[t]==1){
      Data[t,xindex]<-c(1,0,0)
    } else if (Data$object_cat[t]==2){
      Data[t,xindex]<-c(0,1,0)
    } else if (Data$object_cat[t]==3){
      Data[t,xindex]<-c(0,0,1)}
  }

return(Data)
}