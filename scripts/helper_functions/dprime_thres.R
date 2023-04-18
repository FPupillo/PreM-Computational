dprime_thres<- function(n_old, n_new, n_iterations, prob) {
  # This little function outputs the smallest d' value that is above a certain
  # probability level over noise. The way it does is by computing a null distribution
  # of d' values and selects the value that sits at the proportion level passed
  # as an argument.
  # Takes in: 
  # - n_old: number of old items in the memory test.
  # - n_new: number of new items in the memory test.
  # - n_iteration: number of samples you want to simulate.
  # - prob: probability level.
  #
  # Outputs: 
  # - a list with:
  #           thres =   d' value at the probability level in prob
  #           distribution = null distribution of dprimes
  # 
  # Authors: Javier Ortiz-Tudela and Francesco Pupillo (Goethe University)
  # ---------------------------
  # Load libraries
  library(psycho)
  library(dplyr)
  
  # How many trials in total?
  n_trials = n_old + n_new
  
  # Get trial sequence
  trial_seq <- c(rep(1, n_old), rep(0, n_new))
  
  # Initialize d' dataframe
  SDT_out <- data.frame(participant=c(1:n_iterations),
                        dprime=rep(9999,n_iterations),
                        beta=rep(9999,n_iterations))
  
  # I have not found a more elegant way, so I'm looping through n_iterations to get the indeces.
  c=1
  for(cSub in 1:n_iterations){
    
    # Create random distribution
    sim_responses <- sample(c(0, 1), n_trials, prob = c(0.5, 0.5), replace = T)
    
    # Get numbers for each response type
    n_hit <- sum(trial_seq==1 & sim_responses==1)
    n_miss <- sum(trial_seq==1 & sim_responses==0)
    n_fa <-  sum(trial_seq==0 & sim_responses==1)
    n_cr <-  sum(trial_seq==0 & sim_responses==0)
    
    # Compute dprime
    temp <- dprime(n_hit, n_miss, n_fa, n_cr)
    
    # Store dprime
    SDT_out$dprime[c] <- temp$dprime
    c=c+1
  }
  
  # Plot dprimes
  hist(SDT_out$dprime)
  
  # Sort
  sorted_d <- SDT_out %>% 
    arrange(SDT_out$dprime)
  
  # Get threshold
  thres <- sorted_d$dprime[n_iterations * prob]
  return(list("thres" = thres, "distribution" = SDT_out$dprime))
}
