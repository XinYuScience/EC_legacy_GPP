# EC_legacy_GPP

Author: Xin Yu
Email:xyu@bgc-jena.mpg.de

CODE for the manuscript: Widespread but divergent drought legacy effects on gross primary productivity across biomes, submitted to Science Advances
System requirements: no operating system dependence; R version is 4.0.3
Installation guide: no installation is needed

Including: 

###7 .R files for the calculation###

0_Functions:The code is the self-defined functions to support calculation

1_Legacy_calculation_4_each_site.R: The code is to identify drought events and quantify drought legacy effects on GPP for eddy covariance sites

2_create_drought_events_csv.R: The code is to create a .csv file which includes the identified drought events

3_summary_all_sites.R: The code is for post-processing to summarize the legacy effects of all indentifed drought events and quantify potential drivers

4_Extract_ERA5_4SPEI.R: to extract era5_land data at eddy covariance sites for the calculation of SPEI

5_SPEI_calculation_PWM.R: to calculate daily SPEI using Probability-Weighted Moments

6_SPEI_drought_flag: to flag the defined drought events with SPEI

7_causal_analysis_all_biomes.R: The code is to identify the drivers of legacy effects for all biomes using HSIC perm, HSIC gamma, and KCIT

8_causal_analysis_forests.R: The code is to identify the drivers of legacy effects for forests using HSIC perm, HSIC gamma, and KCIT

9_SHAP.R:The code is to calculate the SHAP values of predictors for GPP during legacy and non-legacy periods


###3 .csv files as support information for the calculation###

1_all_sites_data_record.csv: The file includes the information of investigated eddy coraviance sites
2_factors.csv: The file includes the climate, site metric, and plant functional traits of investigated eddy covariance sites
Figure3a.csv: the support information for the Figure 3a

###3 .R files + 1 .pptx file for plotting the main figures###
Figure1.R
Figure2.R
Figure3.R
Figure3.pptx: add circles to Figure3

### supplement ###
.R files to plot the supplementary figures and associated calculation scripts.

######## Demo ############6 files for demo###
Data: demo_DE-Hai.csv

Files required: 

1_Legacy_calculation_4_each_site.R
0_Functions.R
1_all_sites_data_record.csv

Instruction: 
Reproducing all results requires data from all 76 investigated eddy covariance sites. But the core calculation is to quantify the drought legacy effects by running the 1_Legacy_calculation_4_each_site.R.
Here, we provide the instruction on how to quantify drought legacy effects at DE-Hai, a temperate forest in Germany, in the following steps:

i) install required R packages (see 1_Legacy_calculation_4_each_site.R for detailed information)

ii) make sure data and required files are in the same folder

iii) run 1_Legacy_calculation_4_each_site.R

Expected output:

demo_output_DE-Hai_df_QC_GS_1.csv: original and processed variables

demo_output_DE-Hai_droughts_1.csv: selected drought events

demo_output_stat_test_1.csv + demo_DE-Hai_legacy_effects_annual.tiff: figure for legacy effects in the post-drought years at the annual scale and significance level in '.csv' file

demo_output_DE-Hai_diff_legacy_GPP_ending_1.csv: quantified legacy effects in the legacy periods

deom_output_DE-Hai_diff_normal_GPP_ending_1.csv: quantified model uncertainty in the non-legacy periods

Expected run time: 
~10 minutes

######## Instruction for use on own data #########
1) The eddy covariance site data file should be in the standard FLUXNET2015 file format
2) If the site is listed in all_sites_data_record.csv, set p in 1_Legacy_calculation_4_each_site.R according to the order of the site in all_sites_data_record.csv
3) If the site is not listed in all_sites_data_record.csv, add the site information into all_sites_data_record.csv and set p in 1_Legacy_calculation_4_each_site.R according to the order of the site in updated all_sites_data_record.csv
