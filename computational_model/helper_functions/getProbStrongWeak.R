getProbStrongWeak<-function(Data, exp){
#------------------------------------------------------------------------------#
# Function that returns the probability for the strong (and weak) prior 
# conditions, excluding the flat condition
# depending on the setup
#
# INPUT:
#   Data = the data created so far by the likelihood function
#   exp = exp1 or exp2
#
# OUTPUT
#   Dataframe where only the desired scene categories are selected
#
#-----------------------------------------------------------------------------#

if (exp == "exp1"){
  
 DataStrong<- Data$Prob[Data$scene_cat==4| Data$scene_cat==5 | 
                       Data$scene_cat==6]
  
} else if (exp == "exp2"){
  
  DataStrong<- Data$Prob[Data$scn_condition==2| Data$scn_condition==3]

}
  return(DataStrong)
}
  
  