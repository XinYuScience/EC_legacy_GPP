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
library(rstatix)
library(ggpubr)
library(Kendall)

case_run<-'spin_up_00_OzFlux_weekly_gap_QC_mix_duration'
drought_events_list<-read.csv(paste0('/Net/Groups/BGI/people/xyu/legacy_EC/2nd_study/drought_events_list_legacy_11.0_',case_run,'.csv'))

#length(site_list)
for (p in 1:nrow(drought_events_list)) {
  
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
                              df_QC_GS$year<=droughts$year[1]+droughts$legacy_years_length[1])-1]<-1
      df_QC_GS$legacy[which(df_QC_GS$year==droughts$year[2] & df_QC_GS$doy>df_QC_GS$doy[drought_end_2])]<-2
      df_QC_GS$legacy[which(df_QC_GS$year>droughts$year[2] &
                              df_QC_GS$year<=droughts$year[2]+droughts$legacy_years_length[2])-1]<-2
      
      if(drought_events_list$drought_year[p]==droughts$year[1]){
        df_QC_GS$group<-df_QC_GS$legacy
      }else{
        df_QC_GS$group<-ifelse(df_QC_GS$legacy==0,0,ifelse(df_QC_GS$legacy==2,1,2))
      }
    }else{
      temp<-df_QC_GS %>% filter(year==droughts$year) %>% filter(drought==1)
      drought_end<-temp$doy[length(temp$doy)]
      df_QC_GS$legacy[which(df_QC_GS$year==droughts$year & df_QC_GS$doy>df_QC_GS$doy[drought_end])]<-1
      df_QC_GS$legacy[which(df_QC_GS$year>droughts$year &
                              df_QC_GS$year<=droughts$year+droughts$legacy_years_length-1)]<-1
      df_QC_GS$group<-ifelse(df_QC_GS$legacy==0,0,1)
    }
    
    #enhanced vegetation index
    EVI<-read.csv(file = paste0('/Net/Groups/BGI/people/xyu/legacy_EC/EVI_all_sites/',study_site,'.csv'))
    EVI$EVI_non_QC<-EVI$EVI
    EVI$EVI[EVI$EVI_N<max(EVI$EVI_N)*0.7]<-NA
    df_QC_GS$EVI<-EVI$EVI[match(as.Date(df_QC_GS$Date),as.Date(EVI$Date))]
    df_QC_GS$EVI_non_QC<-EVI$EVI_non_QC[match(as.Date(df_QC_GS$Date),as.Date(EVI$Date))]
    vars_rf_GPP<-c('GPP','SW_IN','TA','VPD')
    figures_folder<-''
    df_QC_GS<-anomaly_all(data = df_QC_GS,vars = c('EVI'),site = site,sd=1,figures_folder = figures_folder)
    df_QC_GS<-anomaly_non_QC_all(data = df_QC_GS,vars = c('EVI'),site = site,sd=1,figures_folder = figures_folder)
    ggplot(df_QC_GS)+
      geom_point(aes(doy,EVI_non_QC_Anom,color='non_QC'))+
      geom_point(aes(doy,EVI_Anom))+
      facet_wrap(~year)
    temp_normal_year<-df_QC_GS %>% filter(group==1)
    normal_year<-unique(df_QC_GS$year)[!(unique(df_QC_GS$year) %in% unique(temp_normal_year$year))]
    diff_normal_GPP<-NA
    diff_legacy_GPP<-NA
    var_ex_GPP<-rep(NA,length(normal_year))
    for(k in 1:length(normal_year)){
      uncertainty_year<-normal_year[k]
      diff_GPP<-quantify_legacy_effects_non_QC_uncertainty(data=df_QC_GS,random_normal_year=uncertainty_year,
                                                  figures_folder = figures_folder,site=site,vars = c(vars_rf_GPP,'WAI'),EVI_flag = 1)
      
      var_ex_GPP[k]<-diff_GPP$var_explained
      if (k==1){
        diff_normal_GPP<-diff_GPP$normal
        diff_legacy_GPP<-diff_GPP$legacy
      }else{
        diff_normal_GPP<-rbind(diff_normal_GPP,diff_GPP$normal)
        diff_legacy_GPP<-rbind(diff_legacy_GPP,diff_GPP$legacy)
      }
    }
    diff_legacy_GPP2<-read.csv(paste0('/Net/Groups/BGI/people/xyu/legacy_EC/2nd_study/results_11.0_',case_run,'/',
                                      study_site,'_diff_legacy_GPP_ending_1.csv'))
    ggplot(df_QC_GS)+geom_line(aes(doy,EVI_non_QC_Anom))+facet_wrap(~year)
    diff_EVI<-diff_legacy_GPP %>% group_by(year,doy) %>%
      summarise(GPP_Anom_rf_diff=median(GPP_Anom_rf_diff,na.rm=T)) #%>%
      # ggplot()+
      # geom_line(aes(doy,GPP_Anom_rf_diff))+
      # facet_wrap(~year)+
      # NULL
    diff<-diff_legacy_GPP2 %>% group_by(year,doy) %>%
      summarise(GPP_Anom_rf_diff=median(GPP_Anom_rf_diff,na.rm=T))
    diff$GPP_Anom_rf_diff_EVI<-diff_EVI$GPP_Anom_rf_diff
    ggplot(diff)+
      geom_line(aes(doy,GPP_Anom_rf_diff_EVI,color='EVI'))+
      geom_line(aes(doy,GPP_Anom_rf_diff))+
      facet_wrap(~year)
    folder_path_results<-paste0('/Net/Groups/BGI/people/xyu/legacy_EC/2nd_study/results_11.0_',case_run,'_EVI/')
    if (!dir.exists(folder_path_results)) {
      dir.create(folder_path_results)
    }
    write.csv(diff_normal_GPP,file = paste0(folder_path_results,study_site,'_diff_normal_GPP_ending_1.csv'),row.names = F)
    write.csv(diff_legacy_GPP,file = paste0(folder_path_results,study_site,'_diff_legacy_GPP_ending_1.csv'),row.names = F)
    
  }, error = function(e) {
    outputFile <-file(paste0(rootpath,'/2nd_study/error_EVI/',study_site,'_error.txt'))
    writeLines(as.character(e), outputFile)
    close(outputFile)
  })
  print(p)
}
