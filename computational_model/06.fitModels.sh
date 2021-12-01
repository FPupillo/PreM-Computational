#!/bin/bash
# script to run parmeter estimation for different models

setups="pilot priors flat three"


echo $pwd

# loop through setups
for setup in $setups
do

echo working on $setup

# run the script
Rscript computational_model/fitting_scripts/fitting_script_$setup.R 

done