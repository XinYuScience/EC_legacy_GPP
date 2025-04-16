library(ggplot2)
library(dplyr)
library(rstatix)
rootpath<-'/Net/Groups/BGI/people/xyu/legacy_EC/2nd_writing_GRL/response_code/'

data<-read.csv(paste0(rootpath,'drought_events_list_legacy_11.0_spin_up_00_OzFlux_weekly_gap_QC_mix_duration.csv')) %>%
  select(site,drought_year,legacy_wo_unc) %>%
  rename(legacy=legacy_wo_unc)

data_EVI<-read.csv(paste0(rootpath,'drought_events_list_legacy_11.0_spin_up_00_OzFlux_weekly_gap_QC_mix_duration_GPP_mean_EVI.csv')) %>%
  select(site,drought_year,legacy_wo_unc) %>%
  rename(legacy=legacy_wo_unc)

SPEI_flag<-read.csv(paste0(rootpath,'droughts_SPEI.csv'))
data$SPEI_flag<-SPEI_flag$SPEI_1_check_lag[match(paste0(data$site,data$drought_year),paste0(SPEI_flag$site,SPEI_flag$drought_year))]
data_EVI$SPEI_flag<-SPEI_flag$SPEI_1_check_lag[match(paste0(data_EVI$site,data_EVI$drought_year),paste0(SPEI_flag$site,SPEI_flag$drought_year))]
data$legacy_EVI<-data_EVI$legacy
data$legacy<-ifelse(data$SPEI_flag==1,data$legacy*100,NA)
data$legacy_EVI<-ifelse(data$SPEI_flag==1,data$legacy_EVI*100,NA)

model <- lm(legacy_EVI ~ legacy, data = data)

# Summary of the model to get coefficients and significance
summary(model)

# Extracting coefficients
intercept <- coef(model)[1]

slope <- coef(model)[2]

# Extracting p-value
p_value <- summary(model)$coefficients[2,4]

jpeg(paste0(rootpath,'/legacy_EVI_SPEI.jpg'),width = 1400,height = 1200,units = 'px', res=300)
ggplot(data, aes(x=legacy,y=legacy_EVI)) +
  geom_point(color = '#cbd5e8', size = 3,shape =1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  stat_smooth(method = "lm", se = F,span = 0.5)+
  scale_x_continuous(limits = c(-45, 25)) +
  scale_y_continuous(limits = c(-45, 25)) +
  labs(x = "Legacy effects (%)", y = "Legacy effects including EVI (%)") +
  geom_text(x = 19, y = 18, label = "1:1 line", angle = 45,hjust = 0, vjust = 1, color = "grey") +
  theme_classic()+
  theme(panel.grid = element_blank(),
        axis.line = element_line(size = 0.5),
        plot.title = element_text(hjust = 0.5))+
  annotate("text", x = -10, y = 20, label = paste("y =", round(slope, 2), "*x", round(intercept, 2),
                                                  "\np < 0.001"), size = 5) +
  # coord_equal(clip='off')+
  NULL
dev.off()