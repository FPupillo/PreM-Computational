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

models="dfLR_Instr"

# models="dfLR_Instr dLR_Instr fLR_Instr fLR_Eval"

# for single models
for model in $models
do
echo working on $model

# estimate the parameters (by default with 10 for betalimit and 1 for
# starting point)
Rscript parameter_recovery/ParameterRecovery_$model.R 10 1 $initialQ $setup



done

# print the outputs
#Rscript parameter_recovery/Print.ParameteRecovery.R $setup

done