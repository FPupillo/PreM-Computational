#------------------------------------------------------------------------------#
# function that returns the scene condition depending on the 
# simulated scenes
#------------------------------------------------------------------------------#

getSceneCond<-function(Data){
  #----------------------------------------------------------------------------#
  # function that returns the scene condition depending on the 
  # simulated scenes
  #   INPUT: simulated data
  #   OUTPUT: a variable "scene condition" with the simulated scene cond
  #----------------------------------------------------------------------------#
  if (setup=="exp1"){
    sim$scene_cat<-sim$scene
 
  }else {
    scn_condition<-NA
    sim$scn_condition<-  ifelse(sim$scene==1  | sim$scene ==2, 2, 
           ifelse(sim$scene==3 | sim$scene == 4 , 3, 1 ))
  }

return(sim)
}