library(dplyr)
library(ggplot2)
library(stringr)
library(ggtext)

plot_df<-read.csv('Figure4.csv')
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
  NULL
print(a)


data<-read.csv('') %>% 
  filter(no_record==0) %>% 
  filter(!(site=='DE-Tha' & drought_year == 2006)) %>%
  filter(legacy!=0) %>%
  select(everything())


c<-ggplot(data,aes(x=species_richness,y=legacy*100))+
  geom_point(color='#cbd5e8',size=5)+
  geom_hline(yintercept = 0,linetype='dashed')+
  stat_smooth(method = "loess", se = T,span = 1)+
  ylab('Legacy effects (%)')+
  xlab("Species richness")+
  theme_classic()+
  theme(axis.title.x = element_markdown())+
  theme(text = element_text(size = 14, family = "Arial"),
        axis.text = element_text(size = 14)
  )+
  NULL
print(c)

d<-ggplot(data,aes(x=sd_wood_density,y=legacy*100))+geom_point(color='#cbd5e8',size=5)+
  geom_hline(yintercept = 0,linetype='dashed')+
  stat_smooth(method = "loess", se = T,span = 1)+
  ylab('Legacy effects (%)')+
  xlab(expression('Wood density variability (g cm'^'-3'*')'))+
  theme_classic()+
  theme(text = element_text(size = 14, family = "Arial"),
        axis.text = element_text(size = 14)
  )+
  NULL
print(d)

library(ggrepel)
figure_file<-'.tiff'
tiff(figure_file,width = 7200,height = 8800,units = 'px', res=800)
ggpubr::ggarrange(a,heights = c(3, 1),                                                 
                  ggpubr::ggarrange(c,d, ncol = 2, labels = c("b",'c'), align = "h"), 
                  nrow = 2, 
                  labels = "a"                                        
) 
dev.off()


