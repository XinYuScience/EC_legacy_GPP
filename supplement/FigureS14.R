rootpath<-'/Net/Groups/BGI/people/xyu/legacy_EC/2nd_writing_GRL/response_code/'
data<-read.csv(paste0(rootpath,'legacy_list_SPEI.csv'))
site_info<-read.csv(paste0(rootpath,'1_all_sites_data_record.csv')) %>% filter(site %in% data$site)
site_list<-site_info$site

library(dplyr)
library(ggpubr)
rootpath<-'/Net/Groups/BGI/people/xyu/legacy_EC/2nd_writing_GRL/response_code/'
data<-read.csv(paste0(rootpath,'drought_events_list_legacy_11.0_spin_up_00_OzFlux_weekly_gap_QC_mix_duration.csv'))
SPEI_flag<-read.csv(paste0(rootpath,'droughts_SPEI.csv'))
data$SPEI_flag<-SPEI_flag$SPEI_1_check_lag[match(paste0(data$site,data$drought_year),paste0(SPEI_flag$site,SPEI_flag$drought_year))]
data$legacy<-ifelse(data$SPEI_flag==1,data$legacy,NA)
data<-data %>% filter(!is.na(legacy)) %>% select(site,PFT,legacy)

sd<-1

ntree<-400
mtry<-4
nodesize<-5
case_run<-paste0('OOB_OOB_run_',ntree,'_',mtry,'_',nodesize)

data$OOB<-NA

for (i in 1:nrow(data)) {
  site<-data$site[i]
  files<-list.files(full.names = FALSE, path = paste0(rootpath,case_run),
                    pattern = paste0(site,'_OOB_',sd))
  
  if(length(files)==0){
    next
  }
  OOB<-read.csv(paste0(rootpath,case_run,'/',site,'_OOB_',sd,'.csv'))
  data$OOB[i]<-median(OOB$var_ex,na.rm = T)
}

write.csv(data,paste0(rootpath,'OOB_legacy_effects.csv'),row.names = F)

median_value<-median(data$OOB,na.rm = T)
sd_value<-sd(data$OOB,na.rm=T)
min(data$OOB,na.rm=T)
max(data$OOB,na.rm=T)

figure_file<-paste0(rootpath,'OOB_distribution_legacy_effects.jpg')
jpeg(figure_file,width = 2400,height = 1200,units = 'px', res=300)
# Create histogram using ggplot2
a <- ggplot(data, aes(x = OOB)) +
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "black") +
  geom_vline(xintercept = median_value, color = "red", linetype = "dashed", size = 1) +
  geom_text(aes(x = median_value + 0.05, y = Inf),
            label = paste0(sprintf("%.2f", median_value), '\u00B1', sprintf("%.2f", sd_value)),
            vjust = 2, color = "red") +
  xlab(expression('Out-of-bag'~score~(R^2))) +
  ylab('Number of sites') +
  theme_classic()

# Create scatter plot b (as before)
b <- ggplot(data) +
  geom_point(aes(OOB, legacy * 100)) +
  ylab('Legacy effects (%)') +
  xlab(expression('Out-of-bag'~score~(R^2))) +
  theme_classic() +
  annotate("text", x = 0.6, y = 10, label = "p > 0.05 (HSIC)")
# Combine both plots using ggarrange
ggarrange(a, b, ncol = 2, labels = c('a', 'b'))
dev.off()
