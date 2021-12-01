# Computational modeling of PREMUP studies
Computational modeling of PREMUP data was used to derive trial-by-trial values that can potentially describe the mechanisms through which expectations and violation of expectations influence encoding of specific episodes. These quantities are mainly the expected values and the prediction error. 
On each trial and for each scene, we have as many expected values as there are object categories. The expected values are updated by adding up the previous expected value (at trial t-1) to the prediction error multiplied by the learning rate. 
The prediction error is calculated as the difference between the current event and the expectation of the current event. By contrats, the learning rate is estimated at the participant level as the learning rate that maximize the probability of observing participants' behavioural data. 

In order to comupute the Maximum Likelihood Estimation for each model for each participant, we computed the likelihood of the choice that were related to the **strong prior** and **weak prior** conditions, excluding the **flat prior** condition, as in that condition there were not contingencies to learn. 
The strong and weak priors conditions for each setup were related to the following scenes:
 - Premup pilot: Scene 4,5,6 (scene 1,2,3 are flat priors)
 - Premup priors: Scene 1,2,3,4 
 - Premup flat: scene 1,2 (scene 3,4, are flat)
 - premup three: scene 1,2,3,4 (scene 1,2 flat. scene 3,4, weak, scene 5,6, strong) 
 
### Parameter estimation
The parameter estimation is run in the script "01.runParameterEstimation.sh" 

### Model comparison
After the estimation of the parameters for the different models for each participants, the fit of each model to each participants' data was compared by using BIC and Log likelihood, 
respectively. This is done in the script "02.modelComparison_comparefit.R". 
Winning models are stored in the file "winning_mods.csv". 

### Fitting models
The winning models were finally fit to participants's data with the script "03.fitModels.sh". 
This script uses the functions contained in the "fitting_scripts" folder. Than it prints out .csv files with the model-derived variables. 

### analysis
The analysis files are included in the "analysis-files" folder. 

In "analysis.all.Rmd" files, the analysis of model fitting on encoding phase is shown. These analyses source the files that are contained in the "explore_scripts" folder.
First, <setup>.Rmd files includes all the analysis for the different setups, for winning models according to BIC and LL. They print out html files that can be visualised in internet browsers. 

Only for the priors and the three, in addition to the standard analyses, we are carrying out beta, variance partitioning, and uncertainty analysis. These scripts print files with the different PE (response and observational) in the "output" folder and graphs in the "figures" folder.


In "Analysis uncertainty" files, uncertainty is taken into account. There is one file for pilot and one for three. 