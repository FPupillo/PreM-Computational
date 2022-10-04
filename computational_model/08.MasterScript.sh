#!/bin/bash
# Master 
# Script that wraps up the analyses (to run with "bash")

# run parameter recovery for the two experiments and write the results
bash 01.runParameterRecovery.sh

# run Model recovery for the two experiments
setups="exp1 exp2"
 
for setup in $setups
do
echo working on $setup

# get the initialQ
if [ $setup == "exp1" ]
then
initialQ=0.33
else
initialQ=0.5
fi

echo initial Q = $initialQ

# Run the Model Recovery
Rscript 02.runModelRecoveryBetaPlus1.R 10 1 $initialQ $setup

# Plot the confusion Matrices
Rscript 03.plotConfusionMatrix.R $setup

done

# run Parameter estimation
bash 04.runParameterEstimation.sh 

# now compare simulated vs empirical

for setup in $setups
do
echo working on $setup

# get the initialQ
if [ $setup == "exp1" ]
then
initialQ=0.33
else
initialQ=0.5
fi

echo initial Q = $initialQ

Rscript 05.modelComparison_comparefit.R $setup

done

# fit the models
