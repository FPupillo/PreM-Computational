cumAccbyScene<-function(dataframe, acc){
#--------------------------------------------------------------------------------------------------#
# function to create cumulative accuracy for each scene separately
# INPUT: it takes a dataframe for each scene 
#      acc<-nameof the variable with info about accuracy
# OUTPUT: it returns a dataframe with the cumulative accuracy
#--------------------------------------------------------------------------------------------------#
  
  currlist<-dataframe
  
  # create cumulative accuracy per scene
  # first, loopt through participants
  if (!is.null(currlist$participant)){
  participants<-unique(dataframe$participant)
  }else{participants<-1}
  
  currlistAll<-vector()
  for (participant in participants){
    
  # subset
    if (!is.null(currlist$participant)){
  currfile<- currlist[currlist$participant==participant,]
    } else{
      currfile<-currlist
    }
  # first, create trial number for ordering later on
  currlistPart<-vector()
  # loop now through scenes
  for (s in 1:max(unique(currlist$scene_cat))){
  # first subset by scene
  currlistssub<-currfile[currfile$scene_cat==s,]
  # create trialnum by char
  currlistssub$trialNbyscene<-1:nrow(currlistssub)
  # calculatecumacc
  currlistssub$cumAccbyScene<-cummean(currlistssub[[acc]])
  # append to the dataset
  currlistPart<-rbind(currlistPart, currlistssub)
  }
  
  # append the current participant with the global dataset
  currlistAll<-rbind(currlistAll, currlistPart)
  }
  
  return(currlistAll)
  
}
