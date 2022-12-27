getPredicted<-function(df, exp){
  #----------------------------------------------------------------------------#
  # Function that returns the category predicted by participants
  # and also the contingency of the category predicted by participants
  #   INPUT: df - dataframe object with all data from all paricipants
  #          exp - experiment, 1 vs 2
  #   OUTPUT: dataframe with predicted_category and predicted_contingency
  #           added
  #----------------------------------------------------------------------------#
  
  # how many participants?
  part<-unique(df$participant)
  
  # loop across participants
  all_participants<-list()
  for (p in part){
    
    # subset only participants/'data
    curr_df<-df[df$participant==p,]
    
    if (exp==1){
      
      # create predicted condition
      # for experiment 1, the conditions were:
      unique(curr_df$enc_objCat)
      curr_df$predicted_category<-NA
      curr_df$predicted_contingency<-NA
      
      for (n in 1:nrow(curr_df)){
        
        if (!is.na(curr_df$enc_resp[n])){
          
        if (curr_df$enc_resp[n]==1){
          curr_df$predicted_category[n]<-"HouseObjects"
          if (curr_df$enc_scnCat[n]==4){
            curr_df$predicted_contingency[n]<-"0.80"
            
          } else if (curr_df$enc_scnCat[n]>3){
            curr_df$predicted_contingency[n]<-"0.20"
          } else{    curr_df$predicted_contingency[n]<-"0.33"}
        } else if(curr_df$enc_resp[n]==2){
          curr_df$predicted_category[n]<-"Instruments"
          if (curr_df$enc_scnCat[n]==5){
            curr_df$predicted_contingency[n]<-"0.80"
            
          } else if (curr_df$enc_scnCat[n]>3){
            curr_df$predicted_contingency[n]<-"0.20"
          } else{    curr_df$predicted_contingency[n]<-"0.33"}    
        } else{
          curr_df$predicted_category[n]<-"FruitsAndVegs"
          if (curr_df$enc_scnCat[n]==6){
            curr_df$predicted_contingency[n]<-"0.80"
            
          } else if (curr_df$enc_scnCat[n]>3){
            curr_df$predicted_contingency[n]<-"0.20"
          } else{    curr_df$predicted_contingency[n]<-"0.33"}
          
        }
        
        }
        
      }
      
    } else{
    
    # get the predicted category
    # get left and right on the screen
    right_cat<-curr_df$obj_cat[curr_df$corr_ans=="right"][1]
    left_cat<-curr_df$obj_cat[curr_df$corr_ans=="left"][1]
    
    curr_df$predicted_category<-NA
    for (n in 1:nrow(curr_df)){
      
      if (is.na(curr_df$key_resp_trial.keys[n])) {
        curr_df$predicted_category[n]<-NA
      } else if (curr_df$key_resp_trial.keys[n]=="right"){
        curr_df$predicted_category[n]<-right_cat
    } else if (curr_df$key_resp_trial.keys[n]=="left"){
      curr_df$predicted_category[n]<-left_cat 
    }
    }
    
    # now create a matrix with the probabilities of all the categories
    df_cont<-as.data.frame(matrix(NA, nrow = 6, ncol = 2))
      
    names(df_cont)<-c(left_cat, right_cat)
      
    # loop through the scenes and assign the contingencies to each prob
    for (s in 1:max(curr_df$scn_cat, na.rm =T)){
      
      # subset the scene
      left_cat_cont<-curr_df$trial_cond[curr_df$scn_cat==s &
                           curr_df$obj_cat == left_cat][1]
      right_cat_cont<-curr_df$trial_cond[curr_df$scn_cat==s &
                                          curr_df$obj_cat == right_cat][1]
      
      # assign to the dataframe
      df_cont[s,]<-c(left_cat_cont,right_cat_cont )
      
    }
    
    
    # get predicted contingency
    curr_df$predicted_contingency<-NA
    for (i in 1:nrow(curr_df)){
      
      if (!is.na(curr_df$key_resp_trial.keys[i])){
        response<-curr_df$key_resp_trial.keys[i]
        cat<-ifelse(response == "left",1, 2)
        n_scene<-curr_df$scn_cat[i]
        curr_df$predicted_contingency[i]<-df_cont[n_scene,cat ]
        
      }
    
    
      
    }
    
    
  }
  
    all_participants[[p]]<-curr_df
    
  }
    
  # append
  df_all<-do.call(rbind, all_participants)
  
  return(df_all)
  
  
}
