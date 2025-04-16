library(dplyr)
library(ggplot2)
library(stringr)
library(ggtext)
library(e1071)

rootpath<-'/Net/Groups/BGI/people/xyu/legacy_EC/2nd_writing_SA/response_code/'
data<-read.csv(paste0(rootpath,'drought_events_list_legacy_11.0_spin_up_00_OzFlux_weekly_gap_QC_mix_duration.csv')) %>%
  filter(PFT %in% c('EBF','ENF','DBF','MF')) %>%
  filter(no_record==0) %>% 
  filter(!(site=='DE-Tha' & drought_year == 2006)) %>%
  filter(legacy!=0) %>%
  filter(!is.na(mean_P50)) %>%
  select(everything())
SPEI_flag<-read.csv(paste0(rootpath,'droughts_SPEI.csv'))
data$SPEI_flag<-SPEI_flag$SPEI_1_check_lag[match(paste0(data$site,data$drought_year),paste0(SPEI_flag$site,SPEI_flag$drought_year))]
data$legacy<-ifelse(data$SPEI_flag==1,data$legacy*100,NA)
test<-data %>% filter(!is.na(legacy))

# Sample data (replace with your actual values)
MeanP50 <- test$mean_P50
LegacyEffects <- test$legacy

data <- data.frame(MeanP50, LegacyEffects)

# 2. Mahalanobis Distance (multivariate outlier detection)
center <- colMeans(data)
cov_matrix <- cov(data)
mahal_dist <- mahalanobis(data, center, cov_matrix)
threshold <- qchisq(0.975, df = 2)  # 97.5% confidence for 2 variables

mahal_outliers <- mahal_dist > threshold

# Combine all outliers
data$outlier_IQR <- iqr_outliers_x | iqr_outliers_y
data$outlier_Mahalanobis <- mahal_outliers

print(data)

# Optional: Plot with outliers
library(ggplot2)
jpeg(paste0(rootpath,'/FigureS8.jpg'),width = 1600,height = 1200,units = 'px', res=300)
ggplot(data, aes(x = MeanP50, y = LegacyEffects)) +
  geom_point(aes(color = outlier_Mahalanobis), size = 3) +
  scale_color_manual(values = c('#cbd5e8', "red")) +
  theme_classic() +
  labs(color = "Outlier")+
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylim(-8,10)+
  # stat_smooth(method = "lm", se = F, span = 1) +
  ylab('Legacy effects (%)') +
  xlab("Mean P50 (MPa)") +
  theme_classic() +
  theme(
    axis.title.x = element_text(),
    text = element_text(size = 14, family = "Arial"),
    axis.text = element_text(size = 14),
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.1)))+
  NULL
dev.off()