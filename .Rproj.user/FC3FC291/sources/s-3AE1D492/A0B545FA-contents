#!/bin/bash
# script to run parmeter estimation for different models

setups="exp1 exp2"

echo $pwd
# loop through setups
for setup in $setups
do
echo working on $setup

if [ $setup == "exp1" ]
then
initialQ=0.33
else
initialQ=0.5
fi

echo initial Q = $initialQ
# model1="RescorlaWagner_obsALLnobeta RescorlaWagner_feedb RescorlaWagner_feedbAndQ
#  RescorlaWagner_feedbmin1 RescorlaWagner_obs RescorlaWagner_obsALL
# RescorlaWagner_obsALLdualLR RescorlaWagner_obsALL1nfree RescorlaWagner_obsALL1n"
 
models="dLR_Instr fLR_Instr fLR_Eval"

# for single models
for model in $models
do
echo working on $model

Rscript parameter_estimation/Parameter_estimation_Paral_$setup.R 5 10 $initialQ $model

done


done
