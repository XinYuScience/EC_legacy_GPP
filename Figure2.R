library(rstatix)
library(ggpubr)
library(tidyr)
library(dplyr)
library(cowplot)

data<-read.csv('')

data$group<-ifelse(is.na(data$legacy),'no drought',ifelse(data$legacy>0.00,'positive',ifelse(data$legacy<(-0.00),'negative','no legacy effects')))

data$PFT2<-data$PFT
data$PFT2[data$PFT2=='WSA']<-'SAV'
data$PFT2[data$PFT2 %in% c('CSH','OSH')]<-'SHR'

df<-data %>% dplyr::select(site,concurrent,legacy,PFT2,group) %>% filter(group %in% c('positive','negative','no legacy effects')) %>%
  rename(
    C=concurrent,
    L=legacy,
  ) %>% 
  mutate(N=C+L) %>%
  pivot_longer(-c(site,group,PFT2))

stat.test_a <- df %>%
  filter(group == 'negative') %>%
  wilcox_test(value ~ name,paired = T) %>% add_significance() %>% 
  add_xy_position(x = "name") %>% filter(((group1 == 'C')&(group2 == 'N')))

stat.test_b <- df %>%
  filter(group == 'positive') %>%
  wilcox_test(value ~ name,paired = T) %>% add_significance() %>% 
  add_xy_position(x = "name") %>% filter(((group1 == 'C')&(group2 == 'N')))
stat.test_c <- df %>%
  filter(group == 'no legacy effects') %>%
  wilcox_test(value ~ name,paired = T) %>% add_significance() %>% 
  add_xy_position(x = "name") %>% filter(((group1 == 'C')&(group2 == 'N')))

a<-ggplot(df %>% filter(group == 'negative'),aes(x=name,y=value*100,color=name))+
  geom_boxplot(outlier.colour = 'white')+
  geom_point(aes(x=name,y=value*100,color=name),position = position_jitter(),alpha=0.5)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  scale_color_manual(values = c('#543005',"#A6611A","#DFC27D"),
                     labels = c('concurrent effects','legacy effects','net effects'),
                     name='')+
  stat_pvalue_manual(stat.test_a, label = "p.adj.signif", tip.length = 0.01,y.position = c(5))+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none')+
  facet_wrap(~group,ncol=2,scales = 'free',labeller = as_labeller(c(negative='negative legacy effects')))+
  ylim(-40,22)+
  ylab('GPP anomaly change (%)')+
  xlab('')+
  theme(text = element_text(size = 12, family = "Arial"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12)
        )+
  NULL
print(a)
b<-ggplot(df %>% filter(group == 'positive'),aes(x=name,y=value*100,color=name))+
  geom_boxplot(outlier.colour = 'white')+
  geom_point(aes(x=name,y=value*100,color=name),position = position_jitter(),alpha=0.5)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  scale_color_manual(values = c('#543005',"#A6611A","#DFC27D"),
                     labels = c('concurrent effects','legacy effects','net effects'),
                     name='')+
  stat_pvalue_manual(stat.test_b, label = "p.adj.signif", tip.length = 0.01,y.position = c(16))+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none')+
  facet_wrap(~group,ncol=2,scales = 'free',labeller = as_labeller(c(positive='positive legacy effects')))+
  ylim(-40,22)+
  ylab('GPP anomaly change (%)')+
  xlab('')+
  theme(text = element_text(size = 12, family = "Arial"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12)
  )+
  NULL
c<-ggplot(df %>% filter(group == 'no legacy effects'),aes(x=name,y=value*100,color=name))+
  geom_boxplot(outlier.colour = 'white')+
  geom_point(aes(x=name,y=value*100,color=name),position = position_jitter(),alpha=0.5)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  scale_color_manual(values = c('#543005',"#A6611A","#DFC27D"),
                     labels = c('concurrent effects','legacy effects','net effects'),
                     name='')+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none')+
  facet_wrap(~group,ncol=2,scales = 'free')+
  ylim(-40,22)+
  ylab('GPP anomaly change (%)')+
  xlab('')+
  theme(text = element_text(size = 12, family = "Arial"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12)
  )+
  NULL

figure_file<-'.tiff'
tiff(figure_file,width = 2200,height = 1300,units = 'px', res=300)
ggpubr::ggarrange(a,b,c,ncol=3,labels = c('a','b','c'),common.legend = TRUE, legend = "bottom")
dev.off()
