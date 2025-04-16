#Author: Xin Yu
#Email: xyu@bgc-jena.mpg.de
#The code is to create a .csv file which includes the identified drought events
library(ggplot2)
library(dplyr)

cluster<-T
if(cluster){
  rootpath<-'/Net/Groups/BGI/people/xyu/legacy_EC'
}else{
  rootpath<-'X:/legacy_EC'
}

site_info<-read.csv(paste0(rootpath,'/2nd_writing_SA/response_code/1_all_sites_data_record.csv'))

site_list<-site_info$site

ntree<-400
mtry<-4
nodesize<-4
case_run<-paste0('OOB_run_',ntree,'_',mtry,'_',nodesize)

sd<-1

column_names<-c('site','drought_year','legacy_years_length','no_record')
drought_events_list <- data.frame(matrix(ncol = length(column_names), nrow = 0))
colnames(drought_events_list) <- column_names
for (i in 1:length(site_list)) {
  site<-site_list[i]
  files<-list.files(full.names = FALSE, path = paste0(rootpath,'/2nd_writing_SA/response_code/droughts_',case_run),
                    pattern = paste0(site,'_droughts_',sd)) # daily data's filename
  
  if(length(files)==0){
    temp<-data.frame(
      site=site,
      drought_year=NA,
      legacy_years_length=NA,
      no_record=NA
    )
    drought_events_list<-rbind(drought_events_list,temp)
    next
  }
  droughts<-read.csv(paste0(rootpath,'/2nd_writing_SA/response_code/droughts_',case_run,'/',site,'_droughts_',sd,'.csv'))
  droughts<-droughts[order(droughts$year, decreasing = FALSE),]
  if(nrow(droughts)==1){
    temp<-data.frame(
      site=site,
      drought_year=droughts$year,
      legacy_years_length=droughts$legacy_years_length,
      no_record=droughts$no_record
    )
    drought_events_list<-rbind(drought_events_list,temp)
  }else if(nrow(droughts)==2){
    temp_1<-data.frame(
      site=site,
      drought_year=droughts$year[1],
      legacy_years_length=droughts$legacy_years_length[1],
      no_record=droughts$no_record[1]
    )
    drought_events_list<-rbind(drought_events_list,temp_1)
    temp_2<-data.frame(
      site=site,
      drought_year=droughts$year[2],
      legacy_years_length=droughts$legacy_years_length[2],
      no_record=droughts$no_record[2]
    )
    drought_events_list<-rbind(drought_events_list,temp_2)
  }
}

drought_events_list$PFT<-site_info$PFT[match(drought_events_list$site,site_info$site)]
drought_events_list<-drought_events_list %>% select(site,PFT,everything())
write.csv(drought_events_list,paste0(rootpath,'/2nd_writing_SA/response_code/drought_events_list_',case_run,'.csv'),row.names = F)
