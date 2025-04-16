.libPaths("/Net/Groups/BGI/scratch/xyu/R_libs/x86_64-pc-linux-gnu-library/4.2")
library(ggplot2)
library(dplyr)

rootpath<-'/Net/Groups/BGI/people/xyu/legacy_EC/2nd_writing_GRL/response_code/'
droughts<-read.csv('/Net/Groups/BGI/people/xyu/legacy_EC/2nd_study/drought_events_list_11.0_spin_up_00_OzFlux_weekly_gap_QC_mix_duration.csv') %>%
  tidyr::drop_na()
droughts$SPEI_1_check<-0
droughts$SPEI_1_check_lag<-0
for (i in 1:nrow(droughts)) {
    site<-droughts$site[i]
    df_QC_GS<-read.csv(paste0('/Net/Groups/BGI/people/xyu/legacy_EC/2nd_study/df_QC_GS_11.0_spin_up_00_OzFlux_weekly_gap_QC_mix_duration/',site,'_df_QC_GS_1.csv'))
    SPEI<-read.csv(paste0('/Net/Groups/BGI/people/xyu/SPEI_all_sites/SPEI_',site,'.csv'))
    if(sum(is.na(SPEI$CWD_90))==nrow(SPEI)){
      next
    }
    df_QC_GS$SPEI_90<-SPEI$SPEI_90[match(df_QC_GS$Date,SPEI$Date)]
    df_QC_GS$drought_SPEI_1<-ifelse(df_QC_GS$SPEI_90<(-1),1,0)
    #df_QC_GS$drought_SPEI_1_5<-ifelse(df_QC_GS$SPEI_90<(-1.5),1,0)
    #df_QC_GS$drought_SPEI_2<-ifelse(df_QC_GS$SPEI_90<(-2),1,0)
    
    df_QC_GS$Date<-as.Date(df_QC_GS$Date)
    
    df_drought<-df_QC_GS %>% filter(year==droughts$drought_year[i]) %>% filter(drought==1)
    droughts$SPEI_1_check[i]<-ifelse(sum(df_drought$drought_SPEI_1,na.rm = T)>0,1,0)

    st<-df_drought$Date[1]
    en<-df_drought$Date[nrow(df_drought)]
    
    temp<-df_QC_GS %>% filter(year==droughts$drought_year[i]) %>% filter(Date>=st-15 & Date<=en)
    droughts$SPEI_1_check_lag[i]<-ifelse(sum(temp$drought_SPEI_1,na.rm = T)>0,1,0)
    
    print(i)
}

write.csv(droughts,paste0(rootpath,'droughts_SPEI.csv'),row.names = F)