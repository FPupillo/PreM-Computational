#!/bin/bash
#!/bin/bash
#SBATCH --partition=general1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=80
#SBATCH --mem-per-cpu=1024
#SBATCH --time=04:00:00
#SBATCH --no-requeue
#SBATCH --mail-type=ALL


#Rscript parameter_estimation_byPhase/01.Parameter_estimation_Paral_exp1_learn.R
#Rscript parameter_estimation_byPhase/02.Parameter_estimation_Paral_exp2_learn.R
#Rscript parameter_estimation_byPhase/03.fitting_script_exp1_learning.R
#Rscript parameter_estimation_byPhase/04.fitting_script_exp2_learning.R
#Rscript parameter_estimation_byPhase/05.Parameter_estimation_Paral_exp1_encoding.R
Rscript parameter_estimation_byPhase/Parameter_estimation_Paral_exp2_encoding.R 15 10 0.5 "fLR_Instr"
