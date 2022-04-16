# Changelog

### 17/11/2021
- Finsihed model recovery, paramter estimation, for pilot and three setup
- Created model comparison and model comparison plot#
- attempt at standardizing the names of the model as "RW_obs", "RW_obs_dualLR", "OptimalBayesian". A better attempt would be to have "RW" only, as the terminology "obs" might be misleading and does not appear in the paper
- started modifying the simulatedvsempirical file. It needs more work. We need to equate the number of trials per simulation to the number of actual trial to make it comparable. Read the methods manuscript. 
### 18/11/2021
- created rw_feedb and parameter estimation

### 19/11/2021
- parameter recovery for the feedb model
- impletemented the print data in the script flow (parameterRecovery.sh). 
- modified the name of the parameter recovery and print data to be consistent. 
- created master script that goes to all the processes steps. 

### 22/11/2021
- computed new confusion matrix with beta plus 1. 

### 28/02/2021
- After talking with yee lee, It was clear that we need to add a model where the learning rate decreases, but it is still left as a free parameter and estimated at the participant level. I created the scripts for parameter recovery, model recovery, started them, and started the estimation of the parameters. 

### 03/02/2022
Rerun model recovery with the dfLRI model. The model rerun was beta plus 1, but in the ooutput folder is called model recovery. In case we want to go back to the three model comparison, use the "modelbetaplus1" files.