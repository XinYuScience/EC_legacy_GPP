library(ggplot2)

including<-read.csv('drought_events_list_legacy_11.0_spin_up_00_OzFlux_weekly_gap_QC_mix_duration_GPP_mean_including_drought.csv')

cor(including$legacy,including$legacy_including_drought,use = 'complete.obs')^2

model <- lm(legacy_including_drought ~ legacy, data = including)

# Summary of the model to get coefficients and significance
summary(model)

# Extracting coefficients
intercept <- coef(model)[1]
slope <- coef(model)[2]

tiff('Figures/FigureR_including_drought.tiff',width = 2500,height = 2000,units = 'px', res=400)
ggplot(including, aes(x=legacy*100,y=legacy_including_drought*100)) +
  geom_point(color = '#cbd5e8', size = 3,shape =1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  stat_smooth(method = "lm", se = F,span = 0.5)+
  scale_x_continuous(limits = c(-20, 15)) +
  scale_y_continuous(limits = c(-20, 15)) +
  labs(x = "Legacy effects of one drought based on training set without\n the legacy period of another drought (%)", 
       y = "Legacy effects of one drought based on training set with\n the legacy periods of another drought (%)") +
  geom_text(x = 14, y = 13, label = "1:1 line", angle = 45,hjust = 0, vjust = 1, color = "grey") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(size = 0.5),
        plot.title = element_text(hjust = 0.5))+
  annotate("text", x = -10, y = 12, label = expression("R"^{2}~"= 0.98"), size = 5) +
  annotate("text", x = -10, y = 14, label = paste("y =", round(slope, 2), "*x +", round(intercept, 4)), size = 5) +
  coord_equal(clip='off')+
  NULL
dev.off()