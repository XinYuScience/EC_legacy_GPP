#Author: Xin Yu
#Email: xyu@bgc-jena.mpg.de
#The code is for post-processing to summarize the legacy effects of all indentifed drought events and quantify potential drivers
library(ggplot2)
library(dplyr)
library(tidyr)

cluster<-T
if(cluster){
  rootpath<-'/Net/Groups/BGI/people/xyu/legacy_EC'
}else{
  rootpath<-'X:/legacy_EC'
}

ntree<-400
mtry<-4
nodesize<-4
case_run<-paste0('OOB_run_',ntree,'_',mtry,'_',nodesize)

drought_events_list<-read.csv(paste0(rootpath,'/2nd_writing_SA/response_code/drought_events_list_',case_run,'.csv'))
#legacy
drought_events_list$concurrent<-NA
drought_events_list$legacy<-NA
#drought characteristics
drought_events_list$drought_intensity<-NA
drought_events_list$drought_duration<-NA
drought_events_list$drought_timing<-NA
#pre and post drought conditions
drought_events_list$pre_drought_water_limitation<-NA
drought_events_list$post_drought_water_limitation<-NA
#impact indexes
drought_events_list$impact_peak<-NA


for (i in 1:nrow(drought_events_list)) {
  site<-drought_events_list$site[i]
  tryCatch({
    #if files exist
    files<-list.files(full.names = FALSE, path = paste0(rootpath,'/2nd_writing_SA/response_code/results_',case_run),
                      pattern = paste0(site,'_diff_legacy_GPP_ending')) # daily data's filename
    
    
    if(length(files)==0){
      next
    }
    
    #read dataframe
    df_QC_GS<-read.csv(paste0(rootpath,'/2nd_writing_SA/response_code/df_QC_GS_',case_run,'/',site,'_df_QC_GS_1.csv'))
    mean_seasonal_cycle<-df_QC_GS %>% group_by(doy) %>%
      summarise(GPP=mean(GPP,na.rm=T),
                doy_GS=ifelse(sum(doy_GS,na.rm = T)>0,1,0))
	#obtain the doy of growing season
    GS_length<-sum(mean_seasonal_cycle$doy_GS)
    doy_GS<-which(mean_seasonal_cycle$doy_GS==1)
    st_gs<-doy_GS[1]
	#read legacy effects and uncertainty
    diff_normal<-read.csv(paste0(rootpath,'/2nd_writing_SA/response_code/results_',case_run,'/',site,'_diff_normal_GPP_ending_1.csv'))
    diff_legacy<-read.csv(paste0(rootpath,'/2nd_writing_SA/response_code/results_',case_run,'/',site,'_diff_legacy_GPP_ending_1.csv'))
	#calculate the median of actual and potential GPP anom from multiple runs
    df_legacy_quan<-diff_legacy %>% group_by(year,doy) %>%
      summarise(GPP_Anom_rf = median(GPP_Anom_rf,na.rm = T),
                GPP_Anom = median(GPP_Anom,na.rm=T))
    
    df_normal<-diff_normal %>% select(year,doy,GPP_Anom_rf,GPP_Anom)
    df_obs_pred<-rbind(df_normal,df_legacy_quan)
    
    df_QC_GS$EF_drought<-ifelse(df_QC_GS$EF_Anom<0,1,0)
    df_obs_pred$EF_Anom<-df_QC_GS$EF_Anom[match(paste0(df_obs_pred$year,df_obs_pred$doy),
                                                     paste0(df_QC_GS$year,df_QC_GS$doy))]
    df_obs_pred$EF_drought<-df_QC_GS$EF_drought[match(paste0(df_obs_pred$year,df_obs_pred$doy),
                                                           paste0(df_QC_GS$year,df_QC_GS$doy))]
    df_normal$EF_Anom<-df_QC_GS$EF_Anom[match(paste0(df_normal$year,df_normal$doy),
                                                paste0(df_QC_GS$year,df_QC_GS$doy))]
    df_normal$EF_drought<-df_QC_GS$EF_drought[match(paste0(df_normal$year,df_normal$doy),
                                                      paste0(df_QC_GS$year,df_QC_GS$doy))]
    
    if(length(diff_legacy$Date)==0){
      next
    }
    
    
    stat_test<-read.csv(paste0(rootpath,'/2nd_writing_SA/response_code/stat_test_',case_run,'/',site,'_stat_test_1.csv'))
    stat_test[is.na(stat_test)] <- 0
    
    df_GPP_mean<-df_QC_GS %>% group_by(year) %>%
      summarise(GPP=sum(GPP,na.rm = T),
                GPP_non_QC=sum(GPP_non_QC,na.rm = T))
    
    #concurrent effects
    df_drought<-df_QC_GS %>% filter(year==drought_events_list$drought_year[i]) %>% filter(drought==1) %>%
      filter(doy_GS==1)
    
    drought_events_list$concurrent[i]<-sum(df_drought$GPP_non_QC_Anom,na.rm = T)/mean(df_GPP_mean$GPP_non_QC,na.rm = T)
    
    
    #legacy effects
    diff_normal_median<-diff_normal %>% group_by(doy) %>%
      summarise(diff_GPP_median = median(GPP_Anom_rf_diff,na.rm = T),
                diff_GPP_05 = quantile(GPP_Anom_rf_diff,probs = 0.05,na.rm = T),
                diff_GPP_95 = quantile(GPP_Anom_rf_diff,probs = 0.95,na.rm = T),
                diff_GPP_25 = quantile(GPP_Anom_rf_diff,probs = 0.25,na.rm = T),
                diff_GPP_75 = quantile(GPP_Anom_rf_diff,probs = 0.75,na.rm = T))
    diff_legacy_quan<-diff_legacy %>% group_by(year,doy) %>%
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
    
    
    df_QC_GS$EF_drought<-ifelse(df_QC_GS$EF_Anom<0,1,0)
    diff_legacy_quan$EF_Anom<-df_QC_GS$EF_Anom[match(paste0(diff_legacy_quan$year,diff_legacy_quan$doy),
                                                           paste0(df_QC_GS$year,df_QC_GS$doy))]
    diff_legacy_quan$EF_drought<-df_QC_GS$EF_drought[match(paste0(diff_legacy_quan$year,diff_legacy_quan$doy),
                                                           paste0(df_QC_GS$year,df_QC_GS$doy))]
    diff_legacy_quan<-diff_legacy_quan %>% filter(year<drought_events_list$drought_year[i]+drought_events_list$legacy_years_length[i]) %>%
      filter(year>drought_events_list$drought_year[i]-1)
    
    #legacy effects
    #legacy periods in the drought year missing or not
    if(length(stat_test$p[stat_test$year==drought_events_list$drought_year[i]])==0){
      #legacy years = 0
      if(stat_test$p[stat_test$year==drought_events_list$drought_year[i]+1] > 0.05){
        drought_events_list$legacy[i]<-0
        drought_events_list$legacy_gapped[i]<-0
      }else{
        #legacy years > 0
        legacy_df<-diff_legacy_quan %>% filter(year<drought_events_list$drought_year[i]+drought_events_list$legacy_years_length[i]) %>%
          filter(year>=drought_events_list$drought_year[i])
        drought_events_list$legacy[i]<-sum(legacy_df$legacy_05_95,na.rm = T)/mean(df_GPP_mean$GPP_non_QC,na.rm = T)
      }
    }else{
      #legacy years = 0
      if(length(stat_test$p[stat_test$year==drought_events_list$drought_year[i]+1])==0){
        if(stat_test$p[stat_test$year==drought_events_list$drought_year[i]] > 0.05){
          #legacy periods in the drought year (non-significant)
          drought_events_list$legacy[i]<-0
        }else{
          #significant
          legacy_df<-diff_legacy_quan %>% filter(year<drought_events_list$drought_year[i]+drought_events_list$legacy_years_length[i]) %>%
            filter(year>=drought_events_list$drought_year[i])
          drought_events_list$legacy[i]<-sum(legacy_df$legacy_05_95,na.rm = T)/mean(df_GPP_mean$GPP_non_QC,na.rm = T)
        }
      }else{
        #legacy years > 0
        if(stat_test$p[stat_test$year==drought_events_list$drought_year[i]] > 0.05 &
           (stat_test$p[stat_test$year==drought_events_list$drought_year[i]+1] > 0.05 | drought_events_list$legacy_years_length[i]==1)) {
          #legacy periods in the drought year + the first post-drought year (non-significant)
          drought_events_list$legacy[i]<-0
        }else{
          #entire legacy years (significant)
          legacy_df<-diff_legacy_quan %>% filter(year<drought_events_list$drought_year[i]+drought_events_list$legacy_years_length[i]) %>%
            filter(year>=drought_events_list$drought_year[i])
          
          drought_events_list$legacy[i]<-sum(legacy_df$legacy_05_95,na.rm = T)/mean(df_GPP_mean$GPP_non_QC,na.rm = T)
        }
      }
    }
    
    
########## drought characteristics ###########
    #drought intensity
    WAI_SD_universal<-sd(df_QC_GS$WAI_Anom,na.rm = T)
    drought_events_list$drought_intensity[i]<-quantile(df_drought$WAI_Anom,probs = 0.05,na.rm = T)[[1]]/WAI_SD_universal
    #drought duration
    drought_events_list$drought_duration[i]<-nrow(df_drought)/GS_length
    #drought timing
    drought_events_list$drought_timing[i]<-(df_drought$doy[1]-st_gs)/GS_length
    #predrought condtions
    df_pre_drought<-df_QC_GS %>% filter(year==drought_events_list$drought_year[i]-1)
    drought_events_list$pre_drought_water_limitation[i]<-quantile(df_pre_drought$EF_Anom,probs = 0.05,na.rm = T)[[1]]/EF_SD_universal

    #legacy conditions
    df_post_drought<-df_QC_GS %>% filter((year==drought_events_list$drought_year[i] & doy>df_drought$doy[length(df_drought$doy)]) 
                                         &
                                         year<drought_events_list$drought_year[i]+drought_events_list$legacy_years_length[i])
    drought_events_list$post_drought_water_limitation[i]<-sum(df_post_drought$EF_drought,na.rm = T)/EF_SD_universal
########### impact indexes ##########
    #impact intensity
    drought_events_list$impact_peak[i]<-quantile(df_drought$GPP_non_QC_Anom,probs = 0.05,na.rm = T)[[1]]/
      mean(mean_seasonal_cycle$GPP,na.rm = T)

    
  }, error = function(e) {
    outputFile <-file(paste0(rootpath,'/2nd_writing_SA/response_code/error_summary/',site,'_error.txt'))
    writeLines(as.character(e), outputFile)
    close(outputFile)
  })

  
  print(i)
}

factors<-read.csv(paste0(rootpath,'/2nd_writing_SA/response_code/2_factors.csv'))
########### climate #########
#aridity
drought_events_list$aridity<-factors$aridity[match(drought_events_list$site,factors$site)]
drought_events_list$aridity_zomer<-factors$aridity_zomer[match(drought_events_list$site,factors$site)]
#mean temperature
drought_events_list$mean_T<-factors$mean_T[match(drought_events_list$site,factors$site)]
#precipitation seasonality
drought_events_list$precipitation_seasonality<-factors$precipitation_seasonality[match(drought_events_list$site,factors$site)]
#temperature seasonality
drought_events_list$temperature_seasonality<-factors$temperature_seasonality[match(drought_events_list$site,factors$site)]
########### site metrics ###########
#age
drought_events_list$age<-factors$age[match(drought_events_list$site,factors$site)]
#height
drought_events_list$height<-factors$height[match(drought_events_list$site,factors$site)]
#species richness
drought_events_list$species_richness<-factors$species_richness[match(drought_events_list$site,factors$site)]
#rooting depth
drought_events_list$rooting_depth<-factors$rooting_depth[match(drought_events_list$site,factors$site)]
########### ecosystem-weighted plant hydraulic traits #############
#mean wood density
drought_events_list$mean_wood_density<-factors$mean_wood_density[match(drought_events_list$site,factors$site)]
#standard deviation of wood density
drought_events_list$sd_wood_density<-factors$sd_wood_density[match(drought_events_list$site,factors$site)]
#mean P50
drought_events_list$mean_P50<-factors$mean_P50[match(drought_events_list$site,factors$site)]
#standard deviation of P50
drought_events_list$sd_P50<-factors$sd_P50[match(drought_events_list$site,factors$site)]

#mean HSM50
drought_events_list$mean_HSM50<-factors$mean_HSM50[match(drought_events_list$site,factors$site)]
#standard deviation of HSM50
drought_events_list$sd_HSM50<-factors$sd_HSM50[match(drought_events_list$site,factors$site)]

write.csv(drought_events_list,paste0(rootpath,'/2nd_writing_SA/response_code/drought_legacy_effects_',case_run,'.csv'),row.names = F)
