library(dplyr)
library(ggplot2)
library(stringr)
library(ggtext)

plot_df<-read.csv('/Net/Groups/BGI/people/xyu/legacy_EC/2nd_writing_SA/response_code/Figure3_SA.csv')
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
#a<-a+annotate("text", x = -1, y = -4.5, label = expression('R'^'2'~'= 0.59'), size = 5)
print(a)


library(ggplot2)
library(stringr)
library(ggtext)

rootpath<-'/Net/Groups/BGI/people/xyu/legacy_EC/2nd_writing_SA/response_code/'
data<-read.csv(paste0(rootpath,'drought_events_list_legacy_11.0_spin_up_00_OzFlux_weekly_gap_QC_mix_duration.csv')) %>%
  filter(no_record==0) %>% 
  filter(!(site=='DE-Tha' & drought_year == 2006)) %>%
  filter(legacy!=0) %>%
  select(everything())
SPEI_flag<-read.csv(paste0(rootpath,'droughts_SPEI.csv'))
data$SPEI_flag<-SPEI_flag$SPEI_1_check_lag[match(paste0(data$site,data$drought_year),paste0(SPEI_flag$site,SPEI_flag$drought_year))]
data$legacy<-ifelse(data$SPEI_flag==1,data$legacy*100,NA)

data$type<-ifelse(data$PFT %in% c('EBF','ENF','DBF','MF'),'forest','non-forest')

stat_test_pft<-data %>% group_by(type) %>%
  wilcox_test(legacy~0)
stat_test<-data %>%
  wilcox_test(legacy ~ type,paired = F) %>% add_significance() %>% 
  add_xy_position(x = "type")

b<-ggplot(data,aes(x=type,y=legacy,color=type))+
  geom_boxplot(outlier.colour = 'white')+
  geom_point(aes(x=type,y=legacy,color=type),position = position_jitter(),alpha=0.5)+
  geom_hline(yintercept = 0,linetype='dashed')+
  stat_pvalue_manual(stat_test, label = "p.signif", tip.length = 0.01)+
  theme_classic()+
  scale_color_manual(values = c('#7fc97f','#beaed4'))+
  ylim(-10,17)+
  ylab('Legacy effects (%)')+
  theme(legend.position = 'none')+
  xlab('')+
  theme(text = element_text(size = 14, family = "Arial"),
        axis.text = element_text(size = 14)
  )+
  NULL

test<-data %>% filter(!is.na(legacy)) %>% filter(!is.na(mean_P50)) %>% select(mean_P50,legacy)

center <- colMeans(test)
cov_matrix <- cov(test)
mahal_dist <- mahalanobis(test, center, cov_matrix)
threshold <- qchisq(0.975, df = 2)  # 97.5% confidence for 2 variables

mahal_outliers <- mahal_dist > threshold

# Combine all outliers
test$outlier_Mahalanobis <- mahal_outliers

test<-test %>% filter(outlier_Mahalanobis==FALSE)

model <- lm(test$legacy ~ test$mean_P50)
summary(model)
c<-ggplot(test, aes(x = mean_P50, y = legacy)) +
  geom_point(color = '#cbd5e8', size = 5) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylim(-8,10)+
  stat_smooth(method = "lm", se = T, span = 1) +
  ylab('Legacy effects (%)') +
  xlab("Mean P50 (MPa)") +
  theme_classic() +
  annotate("text",
           x = -4, y = 8,
           label = 'p<0.05',
           size = 5)+
  annotate("text",
           x = -4, y = 6,
           label = expression(R^2 == 0.43),
           #hjust = 1.5, vjust = 1.5,
           size = 5)+
  theme(
    axis.title.x = element_text(),
    text = element_text(size = 14, family = "Arial"),
    axis.text = element_text(size = 14),
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.1)))+
  NULL
print(c)
library(ggrepel)
jpeg(paste0(rootpath,'/Figure3.jpg'),width = 2400,height = 1600,units = 'px', res=200)
ggpubr::ggarrange(a,widths = c(2,1),                                                 
                  ggpubr::ggarrange(b,c, nrow = 2, labels = c("b",'c')), 
                  ncol = 2, 
                  labels = "a"                                        
) 
dev.off()