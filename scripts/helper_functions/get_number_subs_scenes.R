get_number_subs_scenes<-function(df){
#------------------------------------------------------------------------------# 
# function that calculates the number of subsequent same scenes for each image
#
# created on [1] "Sat Sep 10 10:08:24 2022"
#------------------------------------------------------------------------------# 

  dataframe<-df
  
  participants<-unique(dataframe$SubNum)
  
  list_all<-list()
  
  
  # run through participants
  for (part in 1:length(participants)){
    
    # subset the data from the participant
    data_sub<-dataframe[dataframe$SubNum==participants[part],]
    
    # initialize the variable with the subseqeunt scene count
    data_sub$num_subs_scene<-NA
    
    # loop through the items
    for (n in 1:nrow(data_sub)){
      
      # subset only the items that occur after that point
      data_subsequent<-data_sub[(n+1):nrow(data_sub),]
      
      # get the scene category
      curr_scn_cat<-data_sub$scn_cat[n]
      
      # count how many subsequent items of that scene category
      num_subseq_scn_cat<-nrow(data_subsequent[data_subsequent$scn_cat==curr_scn_cat,])
    
      data_sub$num_subs_scene[n]<-num_subseq_scn_cat
      
      }
    
    list_all[[participants[part]]]<-data_sub
    
  } # end loop participants
    
  unlist<-do.call(rbind, list_all)
  
  return(unlist)  
  }