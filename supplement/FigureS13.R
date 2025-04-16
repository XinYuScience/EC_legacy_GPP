library(rstatix)
library(ggpubr)
library(tidyr)
library(dplyr)
library(cowplot)

rootpath<-'/Net/Groups/BGI/people/xyu/legacy_EC/2nd_writing_SA/response_code/'
data<-read.csv(paste0(rootpath,'drought_events_list_legacy_11.0_spin_up_00_OzFlux_weekly_gap_QC_mix_duration.csv'))
SPEI_flag<-read.csv(paste0(rootpath,'droughts_SPEI.csv'))
data$SPEI_flag<-SPEI_flag$SPEI_1_check_lag[match(paste0(data$site,data$drought_year),paste0(SPEI_flag$site,SPEI_flag$drought_year))]
data$legacy<-ifelse(data$SPEI_flag==1,data$legacy,NA)
data$concurrent<-ifelse(data$SPEI_flag==1,data$concurrent,NA)


data$group<-ifelse(is.na(data$legacy),'no drought',ifelse(data$legacy>0.00,'positive',ifelse(data$legacy<(-0.00),'negative','no legacy effects')))

temp<-data %>% filter(group %in% c('positive','negative','no legacy effects'))

median_value<-median(temp$drought_duration,na.rm = T)
sd_value<-sd(temp$drought_duration,na.rm=T)
min(temp$drought_duration,na.rm=T)
max(temp$drought_duration,na.rm=T)

figure_file<-paste0(rootpath,'drought_duration_SPEI.jpg')
jpeg(figure_file,width = 1600,height = 1200,units = 'px', res=300)
hist(temp$drought_duration, 
     main = "", 
     xlab = 'Drought duration (days)', 
     ylab = 'Number of sites',
     col = "lightblue", 
     border = "black")
# Add a red dashed vertical line for the median
abline(v = median_value, 
       col = "red", 
       lwd = 2, 
       lty = 2) # lty = 2 for dashed line

# Add the median value as a label next to the line
text(x = median_value+0.05, 
     y = 10, 
     labels = paste0(sprintf("%.0f", median_value),'\u00B1',sprintf("%.0f", sd_value)), 
     pos = 3, 
     col = "red")
dev.off()