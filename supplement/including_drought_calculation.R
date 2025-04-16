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
source('code_2.0/Functions.R')

#load packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(randomForest)


case_run<-'spin_up_00_OzFlux_weekly_gap_QC_mix_duration'
drought_events_list<-read.csv(paste0('/Net/Groups/BGI/people/xyu/legacy_EC/2nd_study/drought_events_list_legacy_11.0_',case_run,'.csv'))
drought_events_list$legacy_including_drought<-NA
drought_events_list$legacy_wo_unc_including_drought<-NA

#nrow(drought_events_list)
for (p in 1:nrow(drought_events_list)) {
  
  if(is.na(drought_events_list$legacy[p])){
    next
  }else{
    if(drought_events_list$legacy[p]==0){
      next
    }
  }
  study_site<-drought_events_list$site[p]
  
  df_QC_GS<-read.csv(paste0(rootpath,'/2nd_study/df_QC_GS_11.0_',case_run,'/',study_site,'_df_QC_GS_1.csv'))
  df_QC_GS$legacy<-0
  drought_events_list_sub<-drought_events_list %>% filter(site==study_site)
  droughts<-read.csv(paste0(rootpath,'/2nd_study/droughts_11.0_',case_run,'/',study_site,'_droughts_1.csv'))
  droughts<-droughts[order(droughts$drought_events, decreasing = FALSE), ]
  if(nrow(droughts)==1){
    next
  }
  tryCatch({
    
############## separate data into legacy and non-legacy periods###############
    
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
      
      if(drought_events_list$drought_year[p]==droughts$year[1]){
        df_QC_GS$group<-ifelse(df_QC_GS$legacy %in% c(0,2),0,1)
      }else{
        df_QC_GS$group<-ifelse(df_QC_GS$legacy %in% c(0,1),0,1)
      }
    }
    ggplot(df_QC_GS)+
      geom_line(aes(doy,group))+
      facet_wrap(~year)
    temp_normal_year<-df_QC_GS %>% filter(group==1)
    normal_year<-unique(df_QC_GS$year)[!(unique(df_QC_GS$year) %in% unique(temp_normal_year$year))]
    vars_rf_GPP<-c('GPP','SW_IN','TA','VPD')
    figures_folder<-''
    diff_normal_GPP<-NA
    diff_legacy_GPP<-NA
    var_ex_GPP<-rep(NA,length(normal_year))
    for(k in 1:length(normal_year)){
      uncertainty_year<-normal_year[k]
      diff_GPP<-quantify_legacy_effects_non_QC_uncertainty(data=df_QC_GS,random_normal_year=uncertainty_year,
                                                  figures_folder = figures_folder,site=study_site,vars = c(vars_rf_GPP,'WAI'),EVI_flag = 0)
      
      var_ex_GPP[k]<-diff_GPP$var_explained
      if (k==1){
        diff_normal_GPP<-diff_GPP$normal
        diff_legacy_GPP<-diff_GPP$legacy
      }else{
        diff_normal_GPP<-rbind(diff_normal_GPP,diff_GPP$normal)
        diff_legacy_GPP<-rbind(diff_legacy_GPP,diff_GPP$legacy)
        # var_imp_GPP<-rbind(var_imp_GPP,diff_GPP$var_importance)
      }
    }
    
    mean_seasonal_cycle<-df_QC_GS %>% group_by(doy) %>%
      summarise(GPP=mean(GPP,na.rm=T),
                doy_GS=ifelse(sum(doy_GS,na.rm = T)>0,1,0))
    # temp_timing<-which(!is.na(mean_seasonal_cycle$GPP))
    GS_length<-sum(mean_seasonal_cycle$doy_GS)
    doy_GS<-which(mean_seasonal_cycle$doy_GS==1)
    st_gs<-doy_GS[1]
    
    df_GPP_mean<-df_QC_GS %>% group_by(year) %>%
      summarise(GPP=sum(GPP,na.rm = T))
    
    #legacy effects
    diff_normal_median<-diff_normal_GPP %>% group_by(doy) %>%
      summarise(diff_GPP_median = median(GPP_Anom_rf_diff,na.rm = T),
                diff_GPP_05 = quantile(GPP_Anom_rf_diff,probs = 0.05,na.rm = T),
                diff_GPP_95 = quantile(GPP_Anom_rf_diff,probs = 0.95,na.rm = T),
                diff_GPP_25 = quantile(GPP_Anom_rf_diff,probs = 0.25,na.rm = T),
                diff_GPP_75 = quantile(GPP_Anom_rf_diff,probs = 0.75,na.rm = T))
    diff_legacy_quan<-diff_legacy_GPP %>% group_by(year,doy) %>%
      summarise(diff_GPP_median = median(GPP_Anom_rf_diff,na.rm = T),
                diff_GPP_05 = quantile(GPP_Anom_rf_diff,probs = 0.05,na.rm = T),
                diff_GPP_95 = quantile(GPP_Anom_rf_diff,probs = 0.95,na.rm = T))
    
    diff_legacy_quan<-diff_legacy_quan %>% filter(doy %in% doy_GS)
    diff_legacy_quan$uncertainty_median<-diff_normal_median$diff_GPP_median[match(diff_legacy_quan$doy,diff_normal_median$doy)]
    diff_legacy_quan$uncertainty_05<-diff_normal_median$diff_GPP_05[match(diff_legacy_quan$doy,diff_normal_median$doy)]
    diff_legacy_quan$uncertainty_25<-diff_normal_median$diff_GPP_25[match(diff_legacy_quan$doy,diff_normal_median$doy)]
    diff_legacy_quan$uncertainty_75<-diff_normal_median$diff_GPP_75[match(diff_legacy_quan$doy,diff_normal_median$doy)]
    diff_legacy_quan$uncertainty_95<-diff_normal_median$diff_GPP_95[match(diff_legacy_quan$doy,diff_normal_median$doy)]
    
    #out of 5-95%
    diff_legacy_quan$legacy_05_95<-diff_legacy_quan$diff_GPP_median
    diff_legacy_quan$legacy_05_95[(diff_legacy_quan$diff_GPP_median>0 & 
                                     diff_legacy_quan$legacy_05_95<=diff_legacy_quan$uncertainty_95)| 
                                    (diff_legacy_quan$diff_GPP_median<0 & 
                                       diff_legacy_quan$legacy_05_95>=diff_legacy_quan$uncertainty_05)]<-0
    
    diff_legacy_quan$legacy_05_95<-ifelse(diff_legacy_quan$legacy_05_95!=0,ifelse(diff_legacy_quan$diff_GPP_median>0,
                                                                                  diff_legacy_quan$legacy_05_95-diff_legacy_quan$uncertainty_95,
                                                                                  diff_legacy_quan$legacy_05_95-diff_legacy_quan$uncertainty_05),0)
    
    drought_events_list$legacy_including_drought[p]<-sum(diff_legacy_quan$legacy_05_95,na.rm = T)/mean(df_GPP_mean$GPP,na.rm = T)
    drought_events_list$legacy_wo_unc_including_drought[p]<-sum(diff_legacy_quan$diff_GPP_median,na.rm = T)/mean(df_GPP_mean$GPP,na.rm = T)
    
    # diff_legacy<-read.csv(paste0('results_11.0/',study_site,'_diff_legacy_GPP_ending_1.csv'))
    # 
    # diff_legacy_quan2<-diff_legacy %>% group_by(year,doy) %>%
    #   summarise(diff_GPP_median = median(GPP_Anom_smooth_rf_diff,na.rm = T),
    #             diff_GPP_05 = quantile(GPP_Anom_smooth_rf_diff,probs = 0.05,na.rm = T),
    #             diff_GPP_95 = quantile(GPP_Anom_smooth_rf_diff,probs = 0.95,na.rm = T)) %>% 
    #   filter(year %in% c(unique(diff_legacy_quan$year))) %>%
    #   filter(doy>=GS_st) %>% filter(doy<=GS_end)
    # 
    # drought_events_list$R2_including_drought[p]<-cor(diff_legacy_quan$diff_GPP_median,diff_legacy_quan2$diff_GPP_median,use = 'complete.obs')^2
    # 
    # write.csv(diff_normal_GPP,file = paste0('results_11.0/',study_site,'_diff_normal_GPP_ending.csv'),row.names = F)
    # write.csv(diff_legacy_GPP,file = paste0('results_11.0/',study_site,'_diff_legacy_GPP_ending.csv'),row.names = F)
    # 
  }, error = function(e) {
    outputFile <-file(paste0(rootpath,'/2nd_study/error_including_drought/',study_site,'_error.txt'))
    writeLines(as.character(e), outputFile)
    close(outputFile)
  })
  print(p)
}

write.csv(drought_events_list,paste0('/Net/Groups/BGI/people/xyu/legacy_EC/2nd_study/drought_events_list_legacy_11.0_',case_run,'_including_drought.csv'))