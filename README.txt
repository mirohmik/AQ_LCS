### data and code files for the paper ###
### Title : Long-term behavior and stability of calibration models for NO and NO2 low cost sensors
### Author : Horim Kim, Horim Kim, Michael Müller, Stephan Henne, and Christoph Hüglin ###
### https://doi.org/10.5194/amt-15-2979-2022 ###
#########################################

--------------- Version for R and regression packages ---------------
R version 4.1.0

package 'randomForest' ver 4.6-14
package 'MASS' ver 7.3-54

--------------------------- I. Category -----------------------------------------------------------------

- code : This folder contains all the R codes that have been utilized in the data analysis.

- data_input :  This folder contains sensor raw signal database, reference from the Haerkingen station,
		Passive sampler reference during the deployment periods, and malfunction periods.

- data_output : This folder contains intermediate results of the R codes (csv. format), 
		such as imported raw signal data, results of calibration, output of pre-processing, etc. 
		Final results are also contained as csv. format.

-------------------------------- II. Code --------------------------------------------------------------
-------- R codes should run in the order of the folders' number. ---------------------------------------
--------------------------------------------------------------------------------------------------------
---- change setwd() in each R code to appropriate location of the folder -------------------------------
--------------------------------------------------------------------------------------------------------

1_pre_processing : The R codes here do pre-processing the raw signal with reference data, as merging
		   raw signal data and reference data with correct timeline.

2_model_selection : The R code here does model selection which is introduced in Section 3.1.1. of the paper.
		    Figure 3 and Figure 4 is exported from the code.

3_calibration : The R codes here do calibration in each periods (1st colocation, deployment, and 2nd colocation).

		1st_colocation_calibration.R : The code does k-fold cross validation with the data during 1st 
					       colocation period.

		deployment_calibration.R : The code applies the calibration model to deployment period and produces Figure 7.

		2nd_colocation_calibration.R : The code produces timeseries of the calbrated concentration
 					       with raw signal to detect malfunction periods.

4_evaluation_and_filtering 
	
	1st_colocation_calibration_evaluation.R : This code derive the statistical metrics, scatterplots,
						  and timeseries for 1st colocation campaign.

	2nd_colocation_malfunction_filtering.R : This code does filtering the malfunction period during 2nd 
						 colocation campaign, and derive the statistical metrics with 
						 filtered data, scatterplots, and timeseries.

5_Figure : The R codes here produce Figure 5, 6, 8, 9 and Figure s14.




