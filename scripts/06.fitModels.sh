#!/bin/bash
# script to run parmeter estimation for different models

setups="exp1 exp2"


echo $pwd

# loop through setups
for setup in $setups
do

echo working on $setup

# run the script
Rscript scripts/fitting_scripts/fitting_script_$setup.R 

done