
getPE<-function(fittedData,NobjCategories){
#------------------------------------------------------------------------------#
# function that creates the PE
# INPUTS: 
#    fittedData: a data frame with participants' data and 
#                 computational-model-derived PE and values 
#        NobjCategories: number of object categories
# OUTPUTS:
#     a dataframe with the PE
#---------------------------------------------------------------------------#
  
  # ------------------------------------------------------------------------------------
  if (NobjCategories==2){
    fittedData$PE<-NA
    for (o in 1: nrow(fittedData)){
      if (fittedData$object_cat[o]==1){
        fittedData$PE[o]<-fittedData$Delta1[o]
      }else if(fittedData$object_cat[o]==2){
        fittedData$PE[o]<-fittedData$Delta2[o]
      }
    }
  }else{
    fittedData$PE<-NA
    for (o in 1: nrow(fittedData)){
      if (fittedData$object_cat[o]==1){
        fittedData$PE[o]<-fittedData$Delta1[o]
      }else if(fittedData$object_cat[o]==2){
        fittedData$PE[o]<-fittedData$Delta2[o]
      }else if(fittedData$object_cat[o]==3){
        fittedData$PE[o]<-fittedData$Delta3[o]
      }
    }
  }
  
  return(fittedData)
  }