#!/bin/bash
# script to run parmeter estimation for different models of the Rouhani et al (2021)... paper

setups="three"

echo $pwd
# loop through setups
for setup in $setups
do
echo working on $setup
 
#models="RescorlaWagner_decay RescorlaWagner_absPE RescorlaWagner_PEchoice"
models="RescorlaWagner_absPEchoice"

for model in $models
do
echo working on $model

Rscript parameter_estimation/Parameter_estimation_Paral_$setup.Rou.R y 30 0.5 $model

done

done
