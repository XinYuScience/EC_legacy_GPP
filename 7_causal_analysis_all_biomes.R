library(RCIT)
library(kpcalg)
library(energy)
library(dplyr)
library(poolr)
library(jmuOutlier)
rootpath<-'X:/legacy_EC/2nd_writing_GRL/response_code/'
data<-read.csv(paste0(rootpath,'drought_events_list_legacy_11.0_spin_up_00_OzFlux_weekly_gap_QC_mix_duration.csv')) %>%
  filter(no_record==0) %>% 
  filter(!(site=='DE-Tha' & drought_year == 2006)) %>%
  filter(legacy!=0) %>%
  select(everything())
SPEI_flag<-read.csv(paste0(rootpath,'droughts_SPEI.csv'))
data$SPEI_flag<-SPEI_flag$SPEI_1_check_lag[match(paste0(data$site,data$drought_year),paste0(SPEI_flag$site,SPEI_flag$drought_year))]
data$legacy<-ifelse(data$SPEI_flag==1,data$legacy*100,NA)
data<-data %>% dplyr::select(-c(site,drought_year,legacy_years_length,
                 no_record,concurrent_gap,legacy_gap,SPEI_flag))
data$type<-ifelse(data$PFT %in% c('EBF','ENF','MF','DBF'),1,2)
data<-data %>% dplyr::select(-PFT)

legacy<-'legacy'
names<-colnames(data)

p<-rep(0,length(names))
p2<-rep(0,length(names))
p3<-rep(0,length(names))
for (i in 1:length(names)) {
  temp<-data %>% dplyr::select(!!legacy,names[i]) %>% tidyr::drop_na() %>% filter(is.finite(get(names[i])))
  temp<-as.data.frame(sapply(temp, function(data) (data-mean(data,na.rm = T))/sd(data,na.rm = T)))
  p[i]<-U_KCI(temp[[names[i]]],temp[[legacy]])
  p2[i]<-hsic.gamma(temp[[names[i]]],temp[[legacy]],sig = 1)$p.value
  p3[i]<-hsic.perm(temp[[names[i]]],temp[[legacy]],sig = 1)$p.value
}

dt_temp<-data.frame(factor=names,kcit=p,hsic_gamma=p2,hsic_perm=p3)

write.csv(dt_temp,paste0('D:/Legacy_EC/2nd_study/unconditional_test_',legacy,'_nonboot_SPEI.csv'),row.names = F)
