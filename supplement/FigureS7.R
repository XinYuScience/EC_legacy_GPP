library(rstatix)
library(ggpubr)
library(tidyr)
library(dplyr)

Cluster<-T
if(Cluster==T){
  rootpath<-'/Net/Groups/BGI/people/xyu/legacy_EC'
}else(
  rootpath<-'X:/legacy_EC'
)

rootpath<-'/Net/Groups/BGI/people/xyu/legacy_EC/2nd_writing_SA/response_code/'
data<-read.csv(paste0(rootpath,'drought_events_list_legacy_11.0_spin_up_00_OzFlux_weekly_gap_QC_mix_duration.csv'))
SPEI_flag<-read.csv(paste0(rootpath,'droughts_SPEI.csv'))
data$SPEI_flag<-SPEI_flag$SPEI_1_check_lag[match(paste0(data$site,data$drought_year),paste0(SPEI_flag$site,SPEI_flag$drought_year))]
data$legacy<-ifelse(data$SPEI_flag==1,data$legacy,NA)
data$concurrent<-ifelse(data$SPEI_flag==1,data$concurrent,NA)

data$group<-ifelse(is.na(data$legacy),'no drought',ifelse(data$legacy>0.00,'positive',ifelse(data$legacy<(-0.00),'negative','no legacy')))

data<-data %>% filter(group %in% c('positive','negative','no legacy'))

data$PFT2<-data$PFT
data$PFT2[data$PFT2=='WSA']<-'SAV'
data$PFT2[data$PFT2 %in% c('CSH','OSH')]<-'SHR'

df<-data %>% dplyr::select(site,concurrent,legacy,PFT2,group) %>% 
  rename(
    C=concurrent,
    L=legacy,
  ) %>% 
  mutate(N=C+L) %>%
  pivot_longer(-c(site,group,PFT2))

stat.test <- df %>%
  group_by(PFT2) %>% #filter(!(PFT2=='SHR')) %>%
  wilcox_test(value ~ name,paired = T) %>% add_significance() %>% 
  add_xy_position(x = "name") %>% filter(((group1 == 'C')&(group2 == 'N')))

stat.test$p.adj.signif[is.na(stat.test$p.adj)]<-'ns'

figure_file<-paste0(rootpath,'/FigureS_CLN_per_PFT_SPEI.jpg')
jpeg(figure_file,width = 2500,height = 2000,units = 'px', res=300)
ggplot(df,aes(x=name,y=value*100,color=name))+
  geom_boxplot(outlier.colour = 'white')+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  geom_point(aes(x=name,y=value*100,color=name),position = position_jitter(),alpha=0.5)+
  scale_color_manual(values = c('#543005',"#A6611A","#DFC27D"),
                     labels = c('concurrent effects','legacy effects','net effects'),
                     name='')+
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01,y.position = c(15,15,15,20,15,15,15))+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  facet_wrap(~PFT2,ncol=3,scales = 'free')+
  ylim(-40,22)+
  ylab('GPP anomaly change (%)')+
  xlab('')+
  NULL
dev.off()