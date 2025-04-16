case_run<-'spin_up_00_OzFlux_weekly_gap_QC_mix_duration'
data_0.8SD<-read.csv(paste0('drought_events_list_legacy_11.0_',case_run,'_0.8_SD_GPP_mean.csv'))
data_1SD<-read.csv(paste0('drought_events_list_legacy_11.0_',case_run,'_GPP_mean.csv'))
data_1.2SD<-read.csv(paste0('drought_events_list_legacy_11.0_',case_run,'_1.2_SD_GPP_mean.csv'))
data_1.5SD<-read.csv(paste0('drought_events_list_legacy_11.0_',case_run,'_1.5_SD_GPP_mean.csv'))

data_1SD$legacy_0.8SD<-data_0.8SD$legacy[match(paste0(data_1SD$site,data_1SD$drought_year),
                                             paste0(data_0.8SD$site,data_0.8SD$drought_year))]
data_1SD$legacy_1.2SD<-data_1.2SD$legacy[match(paste0(data_1SD$site,data_1SD$drought_year),
                                               paste0(data_1.2SD$site,data_1.2SD$drought_year))]
data_1SD$legacy_1.5SD<-data_1.5SD$legacy[match(paste0(data_1SD$site,data_1SD$drought_year),
                                               paste0(data_1.5SD$site,data_1.5SD$drought_year))]


cor(data_1SD$legacy,data_1SD$legacy_0.8SD,use = 'complete.obs')^2
tiff('Figures/FigureS_0.8SD.tiff',width = 2500,height = 2000,units = 'px', res=400)
a<-ggplot(data_1SD, aes(x=legacy*100,y=legacy_0.8SD*100)) +
  geom_point(color = '#cbd5e8', size = 3,shape =1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(-25, 25)) +
  scale_y_continuous(limits = c(-25, 25)) +
  labs(x = "Legacy effects based on\n -1SD of EF anomalies (%)", y = "Legacy effects based on\n -0.8SD of EF anomalies (%)") +
  geom_text(x = 19, y = 18, label = "1:1 line", angle = 45,hjust = 0, vjust = 1, color = "grey") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(size = 0.5),
        plot.title = element_text(hjust = 0.5))+
  annotate("text", x = -5, y = 18, label = expression("R"^2~"= 0.99"), size = 5) +
  coord_equal(clip='off')+
  NULL
dev.off()

cor(data_1SD$legacy,data_1SD$legacy_1.2SD,use = 'complete.obs')^2
tiff('Figures/FigureS_1.2SD.tiff',width = 2500,height = 2000,units = 'px', res=400)
b<-ggplot(data_1SD, aes(x=legacy*100,y=legacy_1.2SD*100)) +
  geom_point(color = '#cbd5e8', size = 3,shape =1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(-25, 25)) +
  scale_y_continuous(limits = c(-25, 25)) +
  labs(x = "Legacy effects based on\n -1SD of EF anomalies (%)", y = "Legacy effects based on\n -1.2SD of EF anomalies (%)") +
  geom_text(x = 19, y = 18, label = "1:1 line", angle = 45,hjust = 0, vjust = 1, color = "grey") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(size = 0.5),
        plot.title = element_text(hjust = 0.5))+
  annotate("text", x = -5, y = 18, label = expression("R"^2~"= 0.98"), size = 5) +
  coord_equal(clip='off')+
  NULL
dev.off()

cor(data_1SD$legacy,data_1SD$legacy_1.5SD,use = 'complete.obs')^2
tiff('Figures/FigureS_1.5SD.tiff',width = 2500,height = 2000,units = 'px', res=400)
c<-ggplot(data_1SD, aes(x=legacy*100,y=legacy_1.5SD*100)) +
  geom_point(color = '#cbd5e8', size = 3,shape =1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(-25, 25)) +
  scale_y_continuous(limits = c(-25, 25)) +
  labs(x = "Legacy effects based on\n -1SD of EF anomalies (%)", y = "Legacy effects based on\n -1.5SD of EF anomalies (%)") +
  geom_text(x = 19, y = 18, label = "1:1 line", angle = 45,hjust = 0, vjust = 1, color = "grey") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(size = 0.5),
        plot.title = element_text(hjust = 0.5))+
  annotate("text", x = -5, y = 18, label = expression("R"^2~"= 0.89"), size = 5) +
  coord_equal(clip='off')+
  NULL
dev.off()

tiff('Figures/FigureS6_SD_20240611.tiff',width = 1400,height = 500,units = 'px', res=150)
ggpubr::ggarrange(a, b,c, ncol = 3, labels = c("a", "b",'c'))
dev.off()

data_3days_gap<-read.csv(paste0('drought_events_list_legacy_11.0_',case_run,'_3days_gap_GPP_mean.csv'))
data_5days_gap<-read.csv(paste0('drought_events_list_legacy_11.0_',case_run,'_5days_gap_GPP_mean.csv'))


data_1SD$legacy_3days_gap<-data_3days_gap$legacy[match(paste0(data_1SD$site,data_1SD$drought_year),
                                                 paste0(data_3days_gap$site,data_3days_gap$drought_year))]
data_1SD$legacy_5days_gap<-data_5days_gap$legacy[match(paste0(data_1SD$site,data_1SD$drought_year),
                                                       paste0(data_5days_gap$site,data_5days_gap$drought_year))]

cor(data_1SD$legacy,data_1SD$legacy_3days_gap,use = 'complete.obs')^2

a<-ggplot(data_1SD, aes(x=legacy*100,y=legacy_3days_gap*100)) +
  geom_point(color = '#cbd5e8', size = 3,shape =1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(-25, 25)) +
  scale_y_continuous(limits = c(-25, 25)) +
  labs(x = "Legacy effects based on\n less than 4-day gaps (%)", y = "Legacy effects based on\n less than 3-day gaps (%)") +
  geom_text(x = 19, y = 18, label = "1:1 line", angle = 45,hjust = 0, vjust = 1, color = "grey") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(size = 0.5),
        plot.title = element_text(hjust = 0.5))+
  annotate("text", x = -5, y = 18, label = expression("R"^2~"= 0.99"), size = 5) +
  coord_equal(clip='off')+
  NULL
print(a)

cor(data_1SD$legacy,data_1SD$legacy_5days_gap,use = 'complete.obs')^2
b<-ggplot(data_1SD, aes(x=legacy*100,y=legacy_5days_gap*100)) +
  geom_point(color = '#cbd5e8', size = 3,shape =1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(-25, 25)) +
  scale_y_continuous(limits = c(-25, 25)) +
  labs(x = "Legacy effects based on\n less than 4-day gaps (%)", y = "Legacy effects based on\n less than 5-day gaps (%)") +
  geom_text(x = 19, y = 18, label = "1:1 line", angle = 45,hjust = 0, vjust = 1, color = "grey") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(size = 0.5),
        plot.title = element_text(hjust = 0.5))+
  annotate("text", x = -5, y = 18, label = expression("R"^2~"= 0.995"), size = 5) +
  coord_equal(clip='off')+
  NULL
print(b)

tiff('Figures/FigureS7_days_gap_20240611.tiff',width = 1400,height = 800,units = 'px', res=200)
ggpubr::ggarrange(a, b, ncol = 2, labels = c("a", "b"))
dev.off()


data_10days<-read.csv(paste0('drought_events_list_legacy_11.0_',case_run,'_10days_GPP_mean.csv'))
data_20days<-read.csv(paste0('drought_events_list_legacy_11.0_',case_run,'_20days_GPP_mean.csv'))


data_1SD$legacy_10days<-data_10days$legacy[match(paste0(data_1SD$site,data_1SD$drought_year),
                                                 paste0(data_10days$site,data_10days$drought_year))]
data_1SD$legacy_20days<-data_20days$legacy[match(paste0(data_1SD$site,data_1SD$drought_year),
                                                 paste0(data_20days$site,data_20days$drought_year))]

cor(data_1SD$legacy,data_1SD$legacy_10days,use = 'complete.obs')^2
a<-ggplot(data_1SD, aes(x=legacy*100,y=legacy_10days*100)) +
  geom_point(color = '#cbd5e8', size = 3,shape =1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(-25, 25)) +
  scale_y_continuous(limits = c(-25, 25)) +
  labs(x = "Legacy effects based on\n 15-day or longer drought (%)", y = "Legacy effects based on\n 10-day or longer drought (%)") +
  geom_text(x = 19, y = 18, label = "1:1 line", angle = 45,hjust = 0, vjust = 1, color = "grey") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(size = 0.5),
        plot.title = element_text(hjust = 0.5))+
  annotate("text", x = -5, y = 18, label = expression("R"^2~"= 0.999"), size = 5) +
  coord_equal(clip='off')+
  NULL

cor(data_1SD$legacy,data_1SD$legacy_20days,use = 'complete.obs')^2
b<-ggplot(data_1SD, aes(x=legacy*100,y=legacy_20days*100)) +
  geom_point(color = '#cbd5e8', size = 3,shape =1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(-25, 25)) +
  scale_y_continuous(limits = c(-25, 25)) +
  labs(x = "Legacy effects based on\n 15-day or longer drought (%)", y = "Legacy effects based on\n 20-day or longer drought (%)") +
  geom_text(x = 19, y = 18, label = "1:1 line", angle = 45,hjust = 0, vjust = 1, color = "grey") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(size = 0.5),
        plot.title = element_text(hjust = 0.5))+
  annotate("text", x = -5, y = 18, label = expression("R"^2~"= 0.999"), size = 5) +
  coord_equal(clip='off')+
  NULL
tiff('Figures/FigureS8_drought_length_20240611.tiff',width = 1400,height = 800,units = 'px', res=200)
ggpubr::ggarrange(a, b, ncol = 2, labels = c("a", "b"))
dev.off()