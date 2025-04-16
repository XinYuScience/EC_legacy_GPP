#Author: Xin Yu
#Email: xyu@bgc-jena.mpg.de
cluster<-T
if(cluster){
  rootpath<-'/Net/Groups/BGI/people/xyu/legacy_EC'
  datapath<-'/Net/Groups/BGI/people/xyu'
  scratch_path<-'/Net/Groups/BGI/scratch/xyu'
}else{
  rootpath<-'X:/legacy_EC'
  scratch_path<-'Z:/scratch/xyu'
}

#source functions code
source(paste0(rootpath,'/2nd_writing_SA/response_code/0_Functions.R'))

#load packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(randomForest)
library(rstatix)
library(iml)
library(ggpubr)

case_run<-'spin_up_00_OzFlux_weekly_gap_QC_mix_duration'
drought_events_list<-read.csv(paste0('/Net/Groups/BGI/people/xyu/legacy_EC/2nd_study/drought_events_list_legacy_11.0_',case_run,'_GPP_mean.csv'))

drought_events_list$SW_IN_dep<-0
drought_events_list$TA_dep<-0
drought_events_list$VPD_dep<-0
drought_events_list$WAI_dep<-0
drought_events_list$doy_dep<-0

#1:nrow(drought_events_list)
for (p in 1:83) {
  
  if(is.na(drought_events_list$legacy[p])){
    next
  }else{
    if(drought_events_list$legacy[p]==0){
      next
    }
  }
  study_site<-drought_events_list$site[p]
  
  tryCatch({
    
############## separate data into legacy and non-legacy periods###############
    df_QC_GS<-read.csv(paste0(rootpath,'/2nd_study/df_QC_GS_11.0_',case_run,'/',study_site,'_df_QC_GS_1.csv'))
    df_QC_GS$legacy<-0
    drought_events_list_sub<-drought_events_list %>% filter(site==study_site)
    droughts<-read.csv(paste0(rootpath,'/2nd_study/droughts_11.0_',case_run,'/',study_site,'_droughts_1.csv'))
    droughts<-droughts[order(droughts$drought_events, decreasing = FALSE), ]
    if(nrow(droughts)==2){
      temp_1<-df_QC_GS %>% filter(year==droughts$year[1]) %>% filter(drought==1)
      drought_end_1<-temp_1$doy[length(temp_1$doy)]
      temp_2<-df_QC_GS %>% filter(year==droughts$year[2]) %>% filter(drought==1)
      drought_end_2<-temp_2$doy[length(temp_2$doy)]
      df_QC_GS$legacy[which(df_QC_GS$year==droughts$year[1] & df_QC_GS$doy>df_QC_GS$doy[drought_end_1])]<-1
      df_QC_GS$legacy[which(df_QC_GS$year>droughts$year[1] &
                              df_QC_GS$year<=droughts$year[1]+droughts$legacy_years_length[1]-1)]<-1
      df_QC_GS$legacy[which(df_QC_GS$year==droughts$year[2] & df_QC_GS$doy>df_QC_GS$doy[drought_end_2])]<-2
      df_QC_GS$legacy[which(df_QC_GS$year>droughts$year[2] &
                              df_QC_GS$year<=droughts$year[2]+droughts$legacy_years_length[2]-1)]<-2
      
      df_no_legacy<-df_QC_GS %>% filter(legacy==0)
      if(drought_events_list$drought_year[p]==droughts$year[1]){
        df_legacy<-df_QC_GS %>% filter(legacy==1)
      }else{
        df_legacy<-df_QC_GS %>% filter(legacy==2)
      }
    }else{
      temp<-df_QC_GS %>% filter(year==droughts$year) %>% filter(drought==1)
      drought_end<-temp$doy[length(temp$doy)]
      df_QC_GS$legacy[which(df_QC_GS$year==droughts$year & df_QC_GS$doy>df_QC_GS$doy[drought_end])]<-1
      df_QC_GS$legacy[which(df_QC_GS$year>droughts$year &
                              df_QC_GS$year<=droughts$year+droughts$legacy_years_length-1)]<-1
      df_no_legacy<-df_QC_GS %>% filter(legacy==0)
      df_legacy<-df_QC_GS %>% filter(legacy==1)
    }
    
#################### feature importance: legacy vs non-legacy #####################
    #non-legacy period
    df_no_legacy_non_na<-df_no_legacy %>% select(year,doy,GPP_Anom,SW_IN_Anom,TA_Anom,VPD_Anom,WAI_Anom) %>%
      tidyr::drop_na()
    df_no_legacy_non_na$SW_IN_SHAP<-NA
    df_no_legacy_non_na$TA_SHAP<-NA
    df_no_legacy_non_na$VPD_SHAP<-NA
    df_no_legacy_non_na$WAI_SHAP<-NA
    df_no_legacy_non_na$doy_SHAP<-NA
    
    X_no_legacy<-df_no_legacy_non_na %>% select(SW_IN_Anom,TA_Anom,VPD_Anom,WAI_Anom,doy)
    y_no_legacy<-df_no_legacy_non_na$GPP_Anom
    rf_no_legacy<-randomForest(x=X_no_legacy,y=y_no_legacy,ntree=400,na.action = na.exclude,
                     importance=T,mtry=5,nodesize=5,localImp = T)
    
    explainer<-Predictor$new(rf_no_legacy,data = X_no_legacy,y=y_no_legacy)
    shap_no_legacy_values <- matrix(NA, nrow = nrow(df_no_legacy_non_na), ncol = 5)
    colnames(shap_no_legacy_values) <- c("SW_IN_SHAP", "TA_SHAP", "VPD_SHAP", "WAI_SHAP", "doy_SHAP")
    shapley<-Shapley$new(explainer, x.interest = X_no_legacy[1,])

    for (i in 1:nrow(df_no_legacy_non_na)) {
      shapley <- Shapley$new(explainer, x.interest = X_no_legacy[i,])
      shapley$explain(x.interest = X_no_legacy[i,])
      results <- shapley$results
      shap_no_legacy_values[i, ] <- results$phi
    }
    
    df_no_legacy_non_na[c("SW_IN_SHAP", "TA_SHAP", "VPD_SHAP", "WAI_SHAP", "doy_SHAP")] <- shap_no_legacy_values
    
    df_no_legacy_non_na$group<-'non-legacy'
    
    folder_path_SHAP<-paste0('/Net/Groups/BGI/people/xyu/legacy_EC/2nd_study/SHAP_',case_run)
    if (!dir.exists(folder_path_SHAP)) {
      dir.create(folder_path_SHAP)
    }
    
    write.csv(df_no_legacy_non_na,paste0(folder_path_SHAP,'/',study_site,'_',drought_events_list$drought_year[p],'_non_legacy_SHAP.csv'),row.names = F)
    
    rm(shap_no_legacy_values)
    rm(results)
    rm(df_no_legacy_non_na)
    gc()
    
    print('hello')
    
    df_legacy_non_na<-df_legacy %>% select(year,doy,GPP_Anom,SW_IN_Anom,TA_Anom,VPD_Anom,WAI_Anom) %>%
      tidyr::drop_na()
    
    df_legacy_non_na$SW_IN_SHAP<-NA
    df_legacy_non_na$TA_SHAP<-NA
    df_legacy_non_na$VPD_SHAP<-NA
    df_legacy_non_na$WAI_SHAP<-NA
    df_legacy_non_na$doy_SHAP<-NA
    
    X_legacy<-df_legacy_non_na %>% select(SW_IN_Anom,TA_Anom,VPD_Anom,WAI_Anom,doy)
    y_legacy<-df_legacy_non_na$GPP_Anom
    rf_legacy<-randomForest(x=X_legacy,y=y_legacy,ntree=400,na.action = na.exclude,
                               importance=T,mtry=5,nodesize=5,localImp = T)
    explainer_legacy<-Predictor$new(rf_legacy,data = X_legacy,y=y_legacy)
    shap_legacy_values <- matrix(NA, nrow = nrow(df_legacy_non_na), ncol = 5)
    colnames(shap_legacy_values) <- c("SW_IN_SHAP", "TA_SHAP", "VPD_SHAP", "WAI_SHAP", "doy_SHAP")
    
    shapley_legacy<-Shapley$new(explainer_legacy, x.interest = X_legacy[1,])
    for (i in 1:nrow(df_legacy_non_na)) {
      shapley_legacy$explain(x.interest = X_legacy[i,])
      results_legacy<-shapley_legacy$results
      shap_legacy_values[i, ] <- results_legacy$phi
    }
    df_legacy_non_na[c("SW_IN_SHAP", "TA_SHAP", "VPD_SHAP", "WAI_SHAP", "doy_SHAP")] <- shap_legacy_values
    
    df_legacy_non_na$group<-'legacy'
    
    print('hello2')
    
    
    write.csv(df_legacy_non_na,paste0(folder_path_SHAP,'/',study_site,'_',drought_events_list$drought_year[p],'_legacy_SHAP.csv'),row.names = F)
    
    rm(shap_legacy_values)
    rm(results_legacy)
    rm(df_legacy_non_na)
    gc()
    print('hello3')
  }, error = function(e) {
    outputFile <-file(paste0(rootpath,'/2nd_study/error_shap/',study_site,'_error.txt'))
    writeLines(as.character(e), outputFile)
    close(outputFile)
  })
  print(p)
}

