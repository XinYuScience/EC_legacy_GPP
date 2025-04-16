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


#load packages
library(ggplot2)
library(dplyr)
library(ggpubr)
library(boot)

case_run<-'spin_up_00_OzFlux_weekly_gap_QC_mix_duration'
drought_events_list<-read.csv(paste0('/Net/Groups/BGI/people/xyu/legacy_EC/2nd_study/drought_events_list_legacy_11.0_',case_run,'_GPP_mean.csv')) %>% 
  select(everything())
drought_events_list$SW_IN_dep<-0
drought_events_list$TA_dep<-0
drought_events_list$VPD_dep<-0
drought_events_list$WAI_dep<-0
drought_events_list$SW_IN_overlap<-0
drought_events_list$TA_overlap<-0
drought_events_list$VPD_overlap<-0
drought_events_list$WAI_overlap<-0

# Function to perform subsampling from the larger group and then LOESS fitting
loess_pred_subsample <- function(data, indices, x_new, sample_size,span=1) {
  if (length(indices) > sample_size) {
    # Subsample if the current bootstrap sample is from the larger group
    indices <- sample(indices, sample_size, replace = TRUE)
  }
  d <- data[indices,]
  fit <- loess(y ~ x, data = d, span = span)
  predict(fit, x_new)
}

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
    
    df_non_legacy_non_na<-read.csv(paste0('SHAP_',case_run,'/',study_site,'_',drought_events_list$drought_year[p],'_non_legacy_SHAP.csv'))
    df_legacy_non_na<-read.csv(paste0('SHAP_',case_run,'/',study_site,'_',drought_events_list$drought_year[p],'_legacy_SHAP.csv'))
    
    set.seed(123) # For reproducibility
    
    
    functional_response_checking<-function(var,df_legacy_non_na,df_non_legacy_non_na){
      anom_var<-paste0(var,'_Anom')
      shap_var<-paste0(var,'_SHAP')
      
      group1 <- df_legacy_non_na %>% select(!!sym(anom_var), !!sym(shap_var)) %>% rename(x=!!sym(anom_var),y=!!sym(shap_var))
      group2 <- df_non_legacy_non_na %>% select(!!sym(anom_var), !!sym(shap_var)) %>% rename(x=!!sym(anom_var),y=!!sym(shap_var))
      
      common_x <- seq(max(c(min(group1$x), min(group2$x))), min(c(max(group1$x), max(group2$x))), length.out = 1000)
      
      size_of_smaller_group <- nrow(group1)
      
      boot_group1_common_x <- boot(group1, statistic = function(data, indices) loess_pred_subsample(data, indices, common_x, size_of_smaller_group), R = 1000)
      boot_group2_common_x <- boot(group2, statistic = function(data, indices) loess_pred_subsample(data, indices, common_x, size_of_smaller_group), R = 1000)
      
      fit_mean_1<-apply(boot_group1_common_x$t, 2, function(x) mean(x, na.rm = TRUE))
      fit_mean_2<-apply(boot_group2_common_x$t, 2, function(x) mean(x, na.rm = TRUE))
      
      fit_ci_1<-apply(boot_group1_common_x$t, 2, function(x) quantile(x, c(0.025, 0.975),na.rm=T))
      fit_ci_2<-apply(boot_group2_common_x$t, 2, function(x) quantile(x, c(0.025, 0.975),na.rm=T))
      
      # Calculate whether confidence intervals overlap
      # For each point, check if the intervals overlap
      overlap<-rep(0,ncol(fit_ci_1))
      for (i in 1:length(overlap)) {
        ub1<-fit_ci_1[2,i]
        lb1<-fit_ci_1[1,i]
        ub2<-fit_ci_2[2,i]
        lb2<-fit_ci_2[1,i]
        if(ub1 < lb2 || ub2 < lb1){
          overlap[i]<-0
        }else{
          overlap[i]<-1
        }
      }
      overlap_percentage <- sum(overlap)/1000*100
      
      test<-wilcox.test(fit_mean_1,fit_mean_2,paired=T)
      
      return(list(legacy=group1,non_legacy=group2,common_x=common_x,fit_mean_legacy=fit_mean_1,fit_mean_non_legacy=fit_mean_2,
                  fit_ci_legacy=fit_ci_1,fit_ci_non_legacy=fit_ci_2,overlap_percentage=overlap_percentage,p=test$p.value))
    }
    
    
    
    VPD<-functional_response_checking('VPD',df_legacy_non_na,df_non_legacy_non_na)
    SW_IN<-functional_response_checking('SW_IN',df_legacy_non_na,df_non_legacy_non_na)
    WAI<-functional_response_checking('WAI',df_legacy_non_na,df_non_legacy_non_na)
    TA<-functional_response_checking('TA',df_legacy_non_na,df_non_legacy_non_na)
    
    drought_events_list$SW_IN_overlap[p]<-SW_IN$overlap_percentage
    drought_events_list$TA_overlap[p]<-TA$overlap_percentage
    drought_events_list$VPD_overlap[p]<-VPD$overlap_percentage
    drought_events_list$WAI_overlap[p]<-WAI$overlap_percentage
    drought_events_list$SW_IN_dep[p]<-ifelse(SW_IN$p<0.05,1,0)
    drought_events_list$TA_dep[p]<-ifelse(TA$p<0.05,1,0)
    drought_events_list$VPD_dep[p]<-ifelse(VPD$p<0.05,1,0)
    drought_events_list$WAI_dep[p]<-ifelse(WAI$p<0.05,1,0)
    
    color_legacy<-'#7fc97f'
    color_non_legacy<-'#beaed4'
    alpha_plot<-0.05
    a<-ggplot() +
      geom_point(data = VPD$non_legacy, aes(x = x, y = y), color = color_non_legacy, alpha = alpha_plot) +
      geom_point(data = VPD$legacy, aes(x = x, y = y), color = color_legacy, alpha = alpha_plot) +
      geom_line(aes(x = VPD$common_x, y = VPD$fit_mean_legacy, color = 'legacy')) +
      geom_ribbon(aes(x = VPD$common_x, ymin = VPD$fit_ci_legacy[1,], ymax = VPD$fit_ci_legacy[2,]), alpha = 0.5,fill=color_legacy) +
      geom_line(aes(x = VPD$common_x, y = VPD$fit_mean_non_legacy,color='non-legacy')) +
      geom_ribbon(aes(x = VPD$common_x, ymin = VPD$fit_ci_non_legacy[1,], ymax = VPD$fit_ci_non_legacy[2,]), alpha = 0.5,fill=color_non_legacy)+
      labs(x = 'VPD anomalies (hPa)', y = 'SHAP (hPa)')+
      theme_classic()+
      scale_color_manual(labels = c('Legacy','Non-legacy'), values = c(color_legacy,color_non_legacy),name='')+
      annotate(geom="text", x=max(VPD$common_x) * 0.8, y=max(VPD$non_legacy$y) * 0.85, label=paste0(VPD$overlap_percentage,'% overlap'))+
      #annotate(geom="text", x=max(VPD$common_x) * 0.8, y=max(VPD$non_legacy$y) * 0.55, label=ifelse(VPD$p>0.05,'p>0.05','p<0.05'))+
      NULL
    print(a)
    b<-ggplot() +
      geom_point(data = SW_IN$non_legacy, aes(x = x, y = y), color = color_non_legacy, alpha = alpha_plot) +
      geom_point(data = SW_IN$legacy, aes(x = x, y = y), color = color_legacy, alpha = alpha_plot) +
      geom_line(aes(x = SW_IN$common_x, y = SW_IN$fit_mean_legacy, color = 'legacy')) +
      geom_ribbon(aes(x = SW_IN$common_x, ymin = SW_IN$fit_ci_legacy[1,], ymax = SW_IN$fit_ci_legacy[2,]), alpha = 0.5,fill=color_legacy) +
      geom_line(aes(x = SW_IN$common_x, y = SW_IN$fit_mean_non_legacy,color='non-legacy')) +
      geom_ribbon(aes(x = SW_IN$common_x, ymin = SW_IN$fit_ci_non_legacy[1,], ymax = SW_IN$fit_ci_non_legacy[2,]), alpha = 0.5,fill=color_non_legacy)+
      labs(x = expression('SW_IN anomalies (W m'^{-2}*')'), y = expression('SHAP (W m'^{-2}*')'))+
      theme_classic()+
      scale_color_manual(labels = c('Legacy','Non-legacy'), values = c(color_legacy,color_non_legacy),name='')+
      annotate(geom="text", x=max(SW_IN$common_x) * 0.8, y=max(SW_IN$non_legacy$y) * 0.8, label=paste0(SW_IN$overlap_percentage,'% overlap'))+
      #annotate(geom="text", x=max(SW_IN$common_x) * 0.8, y=max(SW_IN$non_legacy$y) * 0.55, label=ifelse(SW_IN$p>0.05,'p>0.05','p<0.05'))+
      NULL
    print(b)
    c<-ggplot() +
      geom_point(data = WAI$non_legacy, aes(x = x, y = y), color = color_non_legacy, alpha = alpha_plot) +
      geom_point(data = WAI$legacy, aes(x = x, y = y), color = color_legacy, alpha = alpha_plot) +
      geom_line(aes(x = WAI$common_x, y = WAI$fit_mean_legacy, color = 'legacy')) +
      geom_ribbon(aes(x = WAI$common_x, ymin = WAI$fit_ci_legacy[1,], ymax = WAI$fit_ci_legacy[2,]), alpha = 0.5,fill=color_legacy) +
      geom_line(aes(x = WAI$common_x, y = WAI$fit_mean_non_legacy,color='non-legacy')) +
      geom_ribbon(aes(x = WAI$common_x, ymin = WAI$fit_ci_non_legacy[1,], ymax = WAI$fit_ci_non_legacy[2,]), alpha = 0.5,fill=color_non_legacy)+
      labs(x = 'WAI anomalies (mm)', y = 'SHAP (mm)')+
      theme_classic()+
      scale_color_manual(labels = c('Legacy','Non-legacy'), values = c(color_legacy,color_non_legacy),name='')+
      annotate(geom="text", x=max(WAI$common_x) * 0.8, y=max(WAI$non_legacy$y) * 0.8, label=paste0(WAI$overlap_percentage,'% overlap'))+
      #annotate(geom="text", x=max(WAI$common_x) * 0.8, y=max(WAI$non_legacy$y) * 0.55, label=ifelse(WAI$p>0.05,'p>0.05','p<0.05'))+
      NULL
    print(c)
    d<-ggplot() +
      geom_point(data = TA$non_legacy, aes(x = x, y = y), color = color_non_legacy, alpha = alpha_plot) +
      geom_point(data = TA$legacy, aes(x = x, y = y), color = color_legacy, alpha = alpha_plot) +
      geom_line(aes(x = TA$common_x, y = TA$fit_mean_legacy, color = 'legacy')) +
      geom_ribbon(aes(x = TA$common_x, ymin = TA$fit_ci_legacy[1,], ymax = TA$fit_ci_legacy[2,]), alpha = 0.5,fill=color_legacy) +
      geom_line(aes(x = TA$common_x, y = TA$fit_mean_non_legacy,color='non-legacy')) +
      geom_ribbon(aes(x = TA$common_x, ymin = TA$fit_ci_non_legacy[1,], ymax = TA$fit_ci_non_legacy[2,]), alpha = 0.5,fill=color_non_legacy)+
      labs(x = 'TA anomalies (degree)', y = 'SHAP (degree)')+
      theme_classic()+
      scale_color_manual(labels = c('Legacy','Non-legacy'), values = c(color_legacy,color_non_legacy),name='')+
      annotate(geom="text", x=max(TA$common_x) * 0.8, y=max(TA$non_legacy$y) * 0.8, label=paste0(TA$overlap_percentage,'% overlap'))+
      #annotate(geom="text", x=max(TA$common_x) * 0.8, y=max(TA$non_legacy$y) * 0.55, label=ifelse(TA$p>0.05,'p>0.05','p<0.05'))+
      NULL
    print(d)
    
    
    tiff(paste0('SHAP_response_20240614/',study_site,'_',drought_events_list$drought_year[p],'.tiff'),width = 1600,height = 1400,units = 'px', res=200)
    plot_all<-ggpubr::ggarrange(a,b,c,d,labels = c('a','b','c','d'),
                      nrow = 2,ncol = 2,common.legend = TRUE, legend = "bottom")
    print(plot_all)
    dev.off()
    }, error = function(e) {
    outputFile <-file(paste0(rootpath,'/2nd_study/error_shap_response/',site,'_error.txt'))
    writeLines(as.character(e), outputFile)
    close(outputFile)
  })
  print(p)
}

write.csv(drought_events_list,'SHAP_response_all_sites_overlap_dep_20240614.csv',row.names = F)

