#!/bin/bash
#SBATCH --partition=general2
#SBATCH --nodes=6
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=40
#SBATCH --mem-per-cpu=1024
#SBATCH --time=06:00:00
#SBATCH --no-requeue
#SBATCH --mail-type=ALL

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
 
models="dfLR_Instr dLR_Instr fLR_Instr fLR_Eval"

# for single models
for model in $models
do
echo working on $model

Rscript parameter_estimation_byPhase/Parameter_estimation_Paral_${setup}_encoding.R 5 10 $initialQ $model &

wait

done

done
