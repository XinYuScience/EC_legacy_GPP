library(dplyr)
rootpath<-'/Net/Groups/BGI/people/xyu/legacy_EC/2nd_writing_SA/response_code/'
data<-read.csv(paste0(rootpath,'drought_events_list_legacy_11.0_spin_up_00_OzFlux_weekly_gap_QC_mix_duration.csv'))
SPEI_flag<-read.csv(paste0(rootpath,'droughts_SPEI.csv'))
data$SPEI_flag<-SPEI_flag$SPEI_1_check_lag[match(paste0(data$site,data$drought_year),paste0(SPEI_flag$site,SPEI_flag$drought_year))]
data$legacy<-ifelse(data$SPEI_flag==1,data$legacy,NA)

all_sites<-read.csv('/Net/Groups/BGI/people/xyu/legacy_EC/all_sites.csv')
data$longitude<-all_sites$longitude[match(data$site,all_sites$site)]
data$latitude<-all_sites$latitude[match(data$site,all_sites$site)]

data$group<-ifelse(is.na(data$legacy),'no drought',ifelse(data$legacy>0.00,'positive',ifelse(data$legacy<(-0.00),'negative','no legacy')))
data$legacy<-ifelse(data$group=='no drought',0,data$legacy)
data<-data %>% filter(!group %in% 'no drought')

data$type<-ifelse(data$PFT %in% c('DBF','EBF','ENF','MF'),'forest',
                  ifelse(data$PFT %in% c('SAV','WSA'),'savanna',
                         ifelse(data$PFT %in% c('CSH','OSH'),'shrubland',
                                'grassland')))
# load library
library(ggplot2)
library(ggpubr)

forest<-data %>% filter(type %in% 'forest')
grassland<-data %>% filter(type %in% 'grassland')
savanna<-data %>% filter(type %in% 'savanna')
shrubland<-data %>% filter(type %in% 'shrubland')

#pos and neg
pos_forest<-forest %>% filter(group=='positive')
neg_forest<-forest %>% filter(group=='negative')
pos_forest_mean<-mean(pos_forest$legacy)
neg_forest_mean<-mean(neg_forest$legacy)
forest_mean<-mean(forest$legacy)

a<-ggplot(forest,aes(legacy))+
  geom_histogram(aes(fill=group),binwidth = 0.01)+
  geom_vline(xintercept = pos_forest_mean,color='#018571',linetype=2)+
  annotate(x=pos_forest_mean+0.02,y=10,label=paste0('~',round(pos_forest_mean*100,1),'%'),vjust=2,geom="label",color='#018571')+
  geom_vline(xintercept = forest_mean,color='black',linetype=2)+
  annotate(x=forest_mean,y=20,label=paste0('~',round(forest_mean*100,1),'%'),vjust=2,geom="label",color='black')+
  geom_vline(xintercept = neg_forest_mean,color='#a6611a',linetype=2)+
  annotate(x=neg_forest_mean-0.02,y=10,label=paste0('~',round(neg_forest_mean*100,1),'%'),vjust=2,geom="label",color='#a6611a')+
  scale_fill_manual(name = '',labels = c('negative','no effect','positive'),
                    values = c('#a6611a','grey45','#018571'),guide='none')+
  theme_classic()+
  ylab('Number of drought events')+
  xlab('Legacy effects')+
  scale_x_continuous(breaks = c(-0.4,-0.2,0,0.2),
                     labels=c('-40%','-20%','0','20%'),
                     limits = c(-0.25,0.25))+
  scale_y_continuous(limits = c(0,20),expand = expansion(mult = c(0, 0.05)))+
  facet_wrap(~type)
print(a)
#pos and neg
pos_grassland<-grassland %>% filter(group=='positive')
neg_grassland<-grassland %>% filter(group=='negative')
pos_grassland_mean<-mean(pos_grassland$legacy)
neg_grassland_mean<-mean(neg_grassland$legacy)
grassland_mean<-mean(grassland$legacy)

GRA_SAV<-rbind(grassland,savanna)

wilcox.test(grassland$legacy)

b<-ggplot(grassland,aes(legacy))+
  geom_histogram(aes(fill=group),binwidth = 0.01)+
  geom_vline(xintercept = pos_grassland_mean,color='#018571',linetype=2)+
  annotate(x=pos_grassland_mean,y=10,label=paste0('~',round(pos_grassland_mean*100,1),'%'),vjust=2,geom="label",color='#018571')+
  geom_vline(xintercept = grassland_mean,color='black',linetype=2)+
  annotate(x=grassland_mean,y=20,label=paste0('~',round(grassland_mean*100,1),'%'),vjust=2,geom="label",color='black')+
  geom_vline(xintercept = neg_grassland_mean,color='#a6611a',linetype=2)+
  annotate(x=neg_grassland_mean,y=10,label=paste0('~',round(neg_grassland_mean*100,1),'%'),vjust=2,geom="label",color='#a6611a')+
  scale_fill_manual(name = '',labels = c('negative','no effect','positive'),
                    values = c('#a6611a','grey45','#018571'),guide='none')+
  theme_classic()+
  ylab('Number of drought events')+
  xlab('Legacy effects')+
  scale_x_continuous(breaks = c(-0.4,-0.2,0,0.2),
                     labels=c('-40%','-20%','0','20%'),
                     limits = c(-0.25,0.25))+
  scale_y_continuous(limits = c(0,20),expand = expansion(mult = c(0, 0.05)))+
  facet_wrap(~type)
print(b)

#pos and neg
pos_savanna<-savanna %>% filter(group=='positive')
neg_savanna<-savanna %>% filter(group=='negative')
pos_savanna_mean<-mean(pos_savanna$legacy)
neg_savanna_mean<-mean(neg_savanna$legacy)
savanna_mean<-mean(savanna$legacy)

c<-ggplot(savanna,aes(legacy))+
  geom_histogram(aes(fill=group),binwidth = 0.01)+
  geom_vline(xintercept = pos_savanna_mean,color='#018571',linetype=2)+
  annotate(x=pos_savanna_mean+0.05,y=10,label=paste0('~',round(pos_savanna_mean*100,1),'%'),vjust=2,geom="label",color='#018571')+
  geom_vline(xintercept = savanna_mean,color='black',linetype=2)+
  annotate(x=savanna_mean,y=20,label=paste0('~',round(savanna_mean*100,1),'%'),vjust=2,geom="label",color='black')+
  geom_vline(xintercept = neg_savanna_mean,color='#a6611a',linetype=2)+
  annotate(x=neg_savanna_mean-0.05,y=10,label=paste0('~',round(neg_savanna_mean*100,1),'%'),vjust=2,geom="label",color='#a6611a')+
  scale_fill_manual(name = '',labels = c('negative','no effect','positive'),
                    values = c('#a6611a','grey45','#018571'),guide='none')+
  theme_classic()+
  ylab('Number of drought events')+
  xlab('Legacy effects')+
  scale_x_continuous(breaks = c(-0.4,-0.2,0,0.2),
                     labels=c('-40%','-20%','0','20%'),
                     limits = c(-0.25,0.25))+
  scale_y_continuous(limits = c(0,20),expand = expansion(mult = c(0, 0.05)))+
  facet_wrap(~type)
print(c)

#pos and neg
pos_shrubland<-shrubland %>% filter(group=='positive')
neg_shrubland<-shrubland %>% filter(group=='negative')
pos_shrubland_mean<-mean(pos_shrubland$legacy)
neg_shrubland_mean<-mean(neg_shrubland$legacy)
shrubland_mean<-mean(shrubland$legacy)

d<-ggplot(shrubland,aes(legacy))+
  geom_histogram(aes(fill=group),binwidth = 0.01)+
  geom_vline(xintercept = shrubland_mean,color='black',linetype=2)+
  annotate(x=shrubland_mean,y=20,label=paste0('~',round(shrubland_mean*100,1),'%'),vjust=2,geom="label",color='black')+
  scale_fill_manual(name = '',labels = c('no effect'),
                    values = c('grey45'),guide='none')+
  theme_classic()+
  ylab('Number of drought events')+
  xlab('Legacy effects')+
  scale_x_continuous(breaks = c(-0.4,-0.2,0,0.2),
                     labels=c('-40%','-20%','0','20%'),
                     limits = c(-0.25,0.25))+
  scale_y_continuous(limits = c(0,20),expand = expansion(mult = c(0, 0.05)))+
  facet_wrap(~type)
print(d)

# #statistics
jpeg(paste0(rootpath,'/CLN_per_PFT_SPEI.jpg'),width = 2000,height = 1200,units = 'px', res=250)
ggpubr::ggarrange(a,b,c,d,ncol=2,nrow = 2,labels = c('a','b','c','d'))
dev.off()

