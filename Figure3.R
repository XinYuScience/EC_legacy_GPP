library(dplyr)
library(ggplot2)
library(stringr)
library(ggtext)

plot_df<-read.csv('Figure3.csv')
plot_df$label_color<-ifelse(plot_df$color==0,'grey20','#d95f02')
plot_df$driver <- factor(plot_df$driver, levels=unique(plot_df$driver))
plot_df$sum_length<-0

plot_df <- plot_df %>%
  mutate(order = row_number())

a<-ggplot(plot_df) +
  geom_text(
    aes(
      x = order,
      y = sum_length, 
      label = str_wrap(driver, 5),
      color = label_color
    ),
    size=5,
    show.legend = FALSE,
    family = "Arial",
  ) +
  scale_color_identity() +
  theme_classic()+
  theme_void()+
  annotate("text", x = 0, y = -4.5, label = "Legacy\neffects", size = 7, color = "red",family = "Arial") +
  coord_polar(clip = "off")+
  theme(axis.text.x = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_blank()
  )+
  theme(plot.margin = unit(c(0.8,0.8,0.8,0.8), "cm"))+
  theme(text = element_text(size = 5, family = "Arial"))+
  NULL
print(a)


data<-read.csv('') %>% 
  filter(no_record==0) %>% 
  filter(!(site=='DE-Tha' & drought_year == 2006)) %>%
  filter(legacy!=0) %>%
  select(everything())

data$type<-ifelse(data$PFT %in% c('EBF','ENF','DBF','MF'),'forest','non-forest')

stat.test <-data.frame(group1='forest',group2='non-forest',p.signif='*') # based on causal analysis

b<-ggplot(data,aes(x=type,y=legacy*100,color=type))+
  geom_boxplot(outlier.colour = 'white')+
  geom_point(aes(x=type,y=legacy*100,color=type),position = position_jitter(),alpha=0.5)+
  geom_hline(yintercept = 0,linetype='dashed')+
  stat_pvalue_manual(stat.test, label = "p.signif", tip.length = 0.01,y.position = c(16))+
  theme_classic()+
  scale_color_manual(values = c('#7fc97f','#beaed4'))+
  ylim(-20,22)+
  ylab('Legacy effects (%)')+
  theme(legend.position = 'none')+
  xlab('')+
  theme(text = element_text(size = 14, family = "Arial"),
        axis.text = element_text(size = 14)
  )+
  NULL
print(b)

library(ggrepel)
figure_file<-'.tiff'
tiff(figure_file,width = 3600,height = 3000,units = 'px', res=400)
ggpubr::ggarrange(a,b,widths = c(3, 1.3),#heights = c(1, 2),                                                 
                  ncol = 2, 
                  labels = c("a","b")
) 
dev.off()