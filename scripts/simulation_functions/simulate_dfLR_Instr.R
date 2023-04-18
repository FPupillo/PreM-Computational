simulate_dfLR_Instr<-function(
  T,
  mu,
  alpha = NULL,
  beta = NULL,
  initialQ = NULL){
  # ---------------------------------------------------------------------------#
  # This function simulates the participants' behaviour at an association task
  # 
  # Input
  #   T<-length of the trials
  #   mu<- probability of the association
  #   alpha<- alpha parameter
  #   beta<- beta parameter
  #   initalQ<- values at which the Qs are initialised
  #
  # Output
  #   dastaframe with $response and $object_cat
  # ---------------------------------------------------------------------------#
  
  # source choose function
  source("helper_functions/chooseMultinom.R")
  source("helper_functions/softmax.R")
  
  # # initialise matrix
  sceneNum<-6
  data<-matrix(NA, nrow= T*sceneNum,ncol=3)
  
  # counter for the matrix
  counter2<-0
  
  for ( s in 1:sceneNum){
    
    # # update counter at every scene
    counter1<-1+counter2
    
    # object category, the category displayed to participants
    object_cat<-sample(c(1:ncol(mu)), T, prob=mu[s,], replace=TRUE)
    
    # initialise Q
    Q<-initialQ[s,]
    
    # initialise a, the choice
    a<-NULL
    
    # initialize r
    r<-NULL
    
    # initialise a three element binary vector idicating which category is shown
    x<-matrix(NA, nrow=T, ncol=ncol(mu))
    
    if (ncol(mu)==3){ 
      for (n in 1:length(object_cat)){
        if (object_cat[n]==1){
          x[n,]<-c(1,0,0)
        } else if ((object_cat[n]==2)){
          x[n,]<-c(0,1,0)
        } else if ((object_cat[n]==3))
          x[n,]<-c(0,0,1)
      }
    }else if (ncol(mu)==2){
      for (n in 1:length(object_cat)){
        if (object_cat[n]==1){
          x[n,]<-c(1,0)
        } else if ((object_cat[n]==2)){
          x[n,]<-c(0,1)
        }
      }
    }
    
    for(t in 1:T){
      
      # compute choice probabilities
      cp<-softmax(Q, beta)

      # make choice according to choice probabilities
      
      if (ncol(mu)==3){
        a[t] <- chooseMultinom(cp)
      } else if (ncol(mu)==2){
        a[t] <- chooseBinomial(cp)
        
      }
      
      # update values
      updateVal<-update(x[t,], Q, alpha/t)
      
      delta<- updateVal$delta

      # update the Q 
      Q <-updateVal$Q
      
    }
    
    choice<-NA
    preference<-NA
    for (e in 1 :length(a)){ 
      if(a[e]==which(mu==max(mu))){choice[e]<-1
      }else{choice[e]<-0}
      preference[e]<-sum(choice)/e
    }
    plot(preference)
    
    # update the counter
    counter2<-counter2+T
    data[counter1:counter2,]<-cbind(rep(s, length=T),a, object_cat)
    counter1<-counter1+T
  }
  data<-data.frame(data)
  names(data)<-c("scene", "a", "object_cat")
  
  return(data)
}
