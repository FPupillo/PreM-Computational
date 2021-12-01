var_murphy<-function(Data, count,t, alphaindex, Varianceindex, xindex){
#------------------------------------------------------------------------------------------------#
# function that estimates the variance as described in Murphy (2012) - Machine learning
# INPuts:
#   Data with participants' behavioural output
# OUTPUS: 
#   Data with the variances
#
#------------------------------------------------------------------------------------------------#
  
  # calculate the variance
  # number of categories
  K<-length(unique(Data$object_cat))
  if (count[Data$scene_cat[t]] ==0) { # if this is the first trial
    alpha_k<-rep(1, ncol(initialQ)) # we are initialising them at 1
  } else {
    alpha_k<-base::colSums(Data[Data$scene_cat==Data$scene_cat[t], xindex], na.rm=T)+rep(1, ncol(initialQ))
  }
  
  # getting the variance (from Murphy, Machine Learning). We use the pseudocounts
  mean_k<-alpha_k/sum(alpha_k)
  
  alpha_0 = sum(alpha_k)
  
  var_k = (alpha_k*(alpha_0-alpha_k))/ (alpha_0^2 * (alpha_0+1))
  
  Data[t, alphaindex]<-alpha_k
  
  Data[t, Varianceindex]<-sum(var_k)
  
  return(Data)
}