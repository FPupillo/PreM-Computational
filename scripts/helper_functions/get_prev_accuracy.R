get_prev_accuracy<-function(df, nTrials){
  #-----------------------------------------------------------------------------#
  # function that calculates how many trials were correct among the previous
  # ones
  #-----------------------------------------------------------------------------#
  
  # how many scenes
  scenes<-unique(df$scn_cat)
  
  # create participant linst
  part_list<-list()
  
  for (part in unique(df$participant)){
    
    curr_df<-df[df$participant==part,]
    
    count<-rep(0,length(scenes))
    
    curr_df$acc_bef<-NA
    
    for (n in 1:nrow(curr_df)){
      
      sceneCounter<-which(scenes==curr_df$scn_cat[n])
      
      if (count[sceneCounter]>nTrials){
        
        # subsert the df with the scene
        df_sub<-curr_df[curr_df$scn_cat==curr_df$scn_cat[n],]
        
        # take the ntrials before
        trial_bef<-df_sub$trial_acc[(count[sceneCounter]-nTrials ): 
                                      (count[sceneCounter] -1)]
        
        # assign it
        curr_df$acc_bef[n]<-paste(as.character(trial_bef), collapse=" ")
        
      }
      
      count[sceneCounter]<-count[sceneCounter]+1 # update the counter
      
    } # end row's loop
    
    part_list[[part]]<-curr_df
    
  } # end participant's oop
  
  unlist<-do.call(rbind, part_list)
  
  return(unlist)
  
}
