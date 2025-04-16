library(dplyr)
library(ggplot2)
library(rstatix)
library(ggpubr)
rootpath<-'/Net/Groups/BGI/people/xyu/legacy_EC/2nd_study'
sd<-1

case_run<-'spin_up_00_OzFlux_weekly_gap_QC_mix_duration'

site<-'US-Var'
drought_legacy_year<-c(2004,2005)
#GPP
diff_normal_GPP<-read.csv(paste0(rootpath,'/results_11.0_',case_run,'/',site,'_diff_normal_GPP_ending_',sd,'.csv'))
diff_legacy_GPP<-read.csv(paste0(rootpath,'/results_11.0_',case_run,'/',site,'_diff_legacy_GPP_ending_',sd,'.csv')) %>% filter(year<2006)
# table(diff_legacy_GPP$year)

normal_median_quantile<-diff_normal_GPP %>% group_by(doy) %>%
  summarize(median_GPP = median(GPP_Anom_rf_diff,na.rm = T),
            quan_05_GPP = quantile(GPP_Anom_rf_diff,probs = 0.05,na.rm = T),
            quan_95_GPP = quantile(GPP_Anom_rf_diff,probs = 0.95,na.rm = T),
            quan_25_GPP = quantile(GPP_Anom_rf_diff,probs = 0.25,na.rm = T),
            quan_75_GPP = quantile(GPP_Anom_rf_diff,probs = 0.75,na.rm = T))
#obtain legacy and uncertainty
legacy_median_quantile<-diff_legacy_GPP %>% group_by(year,doy) %>%
  summarize(median_GPP = median(GPP_Anom_rf_diff,na.rm = T),
            quan_05_GPP = quantile(GPP_Anom_rf_diff,probs = 0.05,na.rm = T),
            quan_95_GPP = quantile(GPP_Anom_rf_diff,probs = 0.95,na.rm = T))

# xlim<-which(!is.na(normal_median_quantile$median_GPP))
# if(length(xlim)==0){
#   xlim_axis<-c(1,366)
# }else{
#   xlim_axis<-c(xlim[1],xlim[length(xlim)])
# }



color_pool<-c('#543005',"#A6611A")
ylab_GPP<-expression('GPP anomaly residuals (gC m'^'-2'~'d'^'-1'*')')

# figures_folder<-paste0(rootpath,'/2nd_writing/figures')
# figure_file<-paste0(figures_folder,'/legacy_GPP_',site,'.tiff')
tiff('Figures/Figure_S_US_Var.tiff',width = 1600,height = 1200,units = 'px', res=200)
plot(diff_normal_GPP$GPP_Anom_rf_diff~diff_normal_GPP$doy,col='white'
     ,type='p',ylab = '',pch=1,xlab='Day of year',main=site,xaxt = "n",cex.lab=1.2,ylim=c(-4,5))
mtext(side = 2, text = ylab_GPP, line = 2) # Adjust the 'line' parameter to control the distance
axis(1, at = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
lines(normal_median_quantile$median_GPP~normal_median_quantile$doy,col='grey4'
      ,type='l',pch=1,lwd=2)
polygon(c(normal_median_quantile$doy[1:164],rev(normal_median_quantile$doy[1:164]))
        ,c(normal_median_quantile$quan_05_GPP[1:164],rev(normal_median_quantile$quan_95_GPP[1:164]))
        ,col = alpha('grey4',0.1),border = NA)
polygon(c(normal_median_quantile$doy[284:366],rev(normal_median_quantile$doy[284:366]))
        ,c(normal_median_quantile$quan_05_GPP[284:366],rev(normal_median_quantile$quan_95_GPP[284:366]))
        ,col = alpha('grey4',0.1),border = NA)
polygon(c(normal_median_quantile$doy[1:164],rev(normal_median_quantile$doy[1:164]))
        ,c(normal_median_quantile$quan_25_GPP[1:164],rev(normal_median_quantile$quan_75_GPP[1:164]))
        ,col = alpha('grey10',0.4),border = NA)
polygon(c(normal_median_quantile$doy[284:366],rev(normal_median_quantile$doy[284:366]))
        ,c(normal_median_quantile$quan_25_GPP[284:366],rev(normal_median_quantile$quan_75_GPP[284:366]))
        ,col = alpha('grey10',0.4),border = NA)
for (i in 1:length(drought_legacy_year)) {
  lines(subset(legacy_median_quantile, year == drought_legacy_year[i])$median_GPP
        ~subset(legacy_median_quantile, year == drought_legacy_year[i])$doy
        ,col=color_pool[i],type='l',lwd=2)
}
legend('topright',ncol=2,
       legend = c('5-95%','25-75%','median',drought_legacy_year),
       col = c(alpha('grey4',0.1),alpha('grey10',0.4),'grey4',color_pool),
       pch = c(NA,NA,NA,rep(NA,length(drought_legacy_year))),
       lwd = c(NA,NA,2,rep(2,length(drought_legacy_year))),cex=1.2,bty = "n",
       fill = c(alpha('grey4',0.1),alpha('grey10',0.4),'white',rep('white',length(drought_legacy_year))),
       border = c(alpha('grey4',0.1),alpha('grey10',0.4),NA,rep(NA,length(drought_legacy_year))))
dev.off()
