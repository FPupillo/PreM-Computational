cumAcc<-function(elements){
# -------------------------------------------------------------------------------------------------#
# function to create cumulative accuracy 
# INPUT: a series of 1 and 0s
# Output: cumulative accuracy at each time point
# -------------------------------------------------------------------------------------------------#
cumacc<-vector()
for ( n in 1: length(elements)){
cumacc[n]<-sum(elements[1:n]/n)
}
return(cumacc)
}
