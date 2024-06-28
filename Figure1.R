library(dplyr)
data<-read.csv('.csv')

all_sites<-read.csv('all_sites.csv')
data$longitude<-all_sites$longitude[match(data$site,all_sites$site)]
data$latitude<-all_sites$latitude[match(data$site,all_sites$site)]

data$group<-ifelse(is.na(data$legacy),'no drought',ifelse(data$legacy>0.00,'positive',ifelse(data$legacy<(-0.00),'negative','no legacy')))
data$legacy<-ifelse(data$group=='no drought',0,data$legacy)
# load library
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf") %>% filter(!(sovereignt=='Antarctica'))

library(ggrepel)

#world map

global<-ggplot() +
        geom_sf(data = world, lwd = 0,fill='gray90') +
        geom_point(data=data,aes(x=longitude,y=latitude,size=abs(legacy),color=group,shape = factor(group)))+
        theme(legend.position = c(0.1, 0.4))+
        theme_bw()+
        coord_sf(expand = FALSE) +
        scale_x_continuous(breaks = seq(from = -180, to = 180, by = 90))+
        scale_y_continuous(breaks = seq(from = -60, to = 80, by = 40))+
     
        scale_size_continuous(name='relative magnitude',breaks = c(0,0.1,0.2),
                              labels = c("0",'10%',"20%"),range=c(1, 5))+
        scale_color_manual(name = '',labels = c('negative','no drought','no legacy','positive'),
                            values = c(alpha('#a6611a',0.7),'black',alpha('grey45',0.7),alpha('#018571',0.7)))+
        scale_shape_manual(name = '',labels = c('negative','no drought','no legacy','positive'),
                           values = c(19,4,19,19))+
        theme(legend.position="right",panel.grid.major = element_line(colour = "transparent"))+
        guides(fill=guide_legend(ncol=1),
               color=guide_legend(ncol=1))+
        xlab('Longitude')+
        ylab('Latitude')+
        theme(text = element_text(size = 12, family = "Arial"),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 12))+

        NULL
print(global)
#pos and neg
pos<-data %>% filter(group=='positive')
neg<-data %>% filter(group=='negative')
pos_mean<-mean(pos$legacy)
neg_mean<-mean(neg$legacy)

test<-data %>% filter(!(group=='no drought'))
overall_mean<-mean(test$legacy)
# #statistics

dis<-ggplot(data %>% filter(!(group=='no drought')),aes(legacy))+
  geom_histogram(aes(fill=group),binwidth = 0.01)+
  geom_vline(xintercept = pos_mean,color=alpha('#018571',0.7),linetype=2)+
  annotate(x=0.10,y=10,label=paste0('~',round(pos_mean*100,1),'%'),vjust=2,geom="label",color=alpha('#018571',0.7))+
  geom_vline(xintercept = overall_mean,color='black',linetype=2)+
  annotate(x=overall_mean,y=20,label=paste0('~',round(overall_mean*100,1),'%'),vjust=2,geom="label",color='black')+
  geom_vline(xintercept = neg_mean,color=alpha('#a6611a',0.7),linetype=2)+
  annotate(x=-0.10,y=10,label=paste0('~',round(neg_mean*100,1),'%'),vjust=2,geom="label",color=alpha('#a6611a',0.7))+
  scale_fill_manual(name = '',labels = c('negative','no effect','positive'),
                     values = c(alpha('#a6611a',0.7),alpha('grey45',0.7),alpha('#018571',0.7)),guide='none')+
  theme_classic()+
  scale_x_continuous(breaks = c(-0.6,-0.4,-0.2,0,0.2,0.4),
                   labels=c('-60%','-40%','-20%','0','20%','40%'))+
  ylab('Number of drought events')+
  xlab('Legacy effects')+
  scale_y_continuous(expand = c(0, 0))+
  theme(text = element_text(size = 12, family = "Arial"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12))+
  NULL
print(dis)

data<-read.csv('') %>% filter(no_record==0)

data$legacy_years_length<-ifelse(data$legacy_years_length==0,NA,data$legacy_years_length-1)

data$group<-ifelse(is.na(data$legacy),'no drought',ifelse(data$legacy>0.00,'positive',ifelse(data$legacy<(-0.00),'negative','no legacy')))

data$PFT2<-data$PFT
data$PFT2[data$PFT2=='WSA']<-'SAV'
data$PFT2[data$PFT2 %in% c('CSH','OSH')]<-'SHR'

df<-data %>% filter(group %in% c('positive','negative')) %>% select(PFT2,legacy_years_length) %>% group_by(PFT2,legacy_years_length) %>%
  summarise(count=length(legacy_years_length))
t<-data %>% filter(group %in% c('positive','negative')) %>% filter(legacy_years_length<=1)
types <- unique(df$PFT2)
values <- 0:4
expanded_df <- expand.grid(PFT2=types, legacy_years_length = values)

# Merge the original dataframe with the expanded one
# and replace NA in number with 0
final_df <- merge(expanded_df, df, by = c("PFT2", "legacy_years_length"), all.x = TRUE) %>%
  replace(is.na(.), 0)
duration<-ggplot(final_df, aes(fill=PFT2, y=count, x=legacy_years_length)) + 
  geom_bar(position=position_dodge(width=0.7), stat="identity", width=0.5) +
  scale_fill_manual(values = c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0','#f0027f','#bf5b17'), name='') +
  geom_text(aes(label=ifelse(count == 0, NA, count), y=count), position=position_dodge(width=0.7), vjust=-0.25)+
  xlab('Years post drought')+
  ylab('Number of drought events')+
  theme_classic()+
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
  theme(text = element_text(size = 12, family = "Arial"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12))+
  NULL

figure_file<-'.tiff'
tiff(figure_file,width = 2400,height = 1800,units = 'px', res=250)
ggpubr::ggarrange(global,
                  ggpubr::ggarrange(duration,dis,ncol=2,labels = c('b','c')),
                  nrow = 2,
                  labels = 'a',
                  heights = c(6, 5))
dev.off()

