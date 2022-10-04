getPstay<-function(Data){
  #----------------------------------------------------------------------------#
  # Function that calculates the probability of switching
  # It needs the category of the response coded in the "respCat" variable
  # 
  # INPUT: data - dataframe
  # OUTPUT: dataframe with a variable (stayShift) indicating whether the decision 
  #         changed as a function of the previous decision (1 = stay, 0 = change)
  #----------------------------------------------------------------------------#
  
  All_data<-list()
  
  for (participant in unique(Data$participant)){
    
    currData<-Data[Data$participant==participant,]
    
    # initialize a variable for the choice
    choice<-vector(mode="numeric", length = 6)
    
    currData$stayShift<-NA
    
    for (n in 1: nrow(currData)){
      
      # which is the current choice?
      currChoice<-as.character(currData$obj_cat_num[n])
      
      # which scence
      scene<-currData$scn_cat[n]
      
      if (n==1){
        choice[scene]<-currChoice
        
      } else{ # if this is not the first trial
        
        if(length(currChoice)>0 & !is.na(choice[scene])){
          
          if (currChoice==choice[scene]){ # if the choice is similar at 
            # the previous choice (for the same character)
            currData$stayShift[n]<-1# stay behaviour
            choice[scene]<-currChoice
          }else{
            currData$stayShift[n]<-0 # shift
            choice[scene]<-currChoice
            
          }
          
        }else{
          choice[character]<-NA
          
          currData$stayShift[n]<-NA
        } 
        
      }
      
    } # end rows' loop
    
    All_data[[participant]]<-currData
    
    
  } # end participant's loop
  
  unlist<-do.call(rbind, All_data)
  
  return(unlist)
  
}
    