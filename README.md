# EC_legacy_GPP

Author: Xin Yu
Email:xyu@bgc-jena.mpg.de

CODE for the manuscript: Widespread but divergent drought legacy effects on gross primary productivity

Including: 

###7 .R files for the calculation###

1_Legacy_calculation_4_each_site.R: The code is to identify drought events and quantify drought legacy effects on GPP for eddy covariance sites

2_create_drought_events_csv.R: The code is to create a .csv file which includes the identified drought events

3_summary_all_sites.R: The code is for post-processing to summarize the legacy effects of all indentifed drought events and quantify potential drivers

4_causal_analysis_all_biomes.R: The code is to identify the drivers of legacy effects for all biomes using HSIC perm, HSIC gamma, and KCIT

5_causal_analysis_forests.R: The code is to identify the drivers of legacy effects for forests using HSIC perm, HSIC gamma, and KCIT

6_SVR.R:The code is to develop a support vector regression model to evaluate the relationship 
#between legacy effects in forests and their drivers, i.e. species richness and wood density variability

7_SHAP.R:The code is to calculate the SHAP values of predictors for GPP during legacy and non-legacy periods

8_Functions.R:The code is the self-defined functions to support calculation


###2 .csv files as support information for the calculation###

1_all_sites_data_record.csv: The file includes the information of investigated eddy coraviance sites
2_factors.csv: The file includes the climate, site metric, and plant functional traits of investigated eddy covariance sites

###4 .R files for plotting the main figures###
