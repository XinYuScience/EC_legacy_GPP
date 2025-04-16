library(dyplr)
library(ggplot2)
rootpath<-'/Net/Groups/BGI/people/xyu/legacy_EC/2nd_writing_GRL/response_code/'
data<-read.csv(paste0(rootpath,'drought_events_list_legacy_11.0_spin_up_00_OzFlux_weekly_gap_QC_mix_duration.csv'))
SPEI_flag<-read.csv(paste0(rootpath,'droughts_SPEI.csv'))
data$SPEI_flag<-SPEI_flag$SPEI_1_check_lag[match(paste0(data$site,data$drought_year),paste0(SPEI_flag$site,SPEI_flag$drought_year))]
data$legacy<-ifelse(data$SPEI_flag==1,data$legacy,NA)
data$concurrent<-ifelse(data$SPEI_flag==1,data$concurrent,NA)


data$group<-ifelse(is.na(data$legacy),'no drought',ifelse(data$legacy>0.00,'positive',ifelse(data$legacy<(-0.00),'negative','no legacy effects')))

df_legacy<-data %>% filter(group %in% c('positive','negative')) %>% 
  select(site,drought_year,legacy_years_length,no_record,legacy) %>% tidyr::drop_na() %>% filter(legacy!=0)

# Initialize an empty list to combine all data for plots
data_for_plot <- list()

# Loop through each row in df_legacy
for (i in 1:nrow(df_legacy)) {
  site <- df_legacy$site[i]
  drought_year <- df_legacy$drought_year[i]
  site_drought<-paste0(site,' ',drought_year)
  legacy_years <- seq(from = df_legacy$drought_year[i], 
                      by = 1, 
                      length.out = df_legacy$legacy_years_length[i])
  
  # Read data
  diff_normal <- read.csv(paste0(rootpath, '/results_OOB_run_400_4_5/', site, '_diff_normal_GPP_ending_1.csv'))
  diff_legacy <- read.csv(paste0(rootpath, '/results_OOB_run_400_4_5/', site, '_diff_legacy_GPP_ending_1.csv'))
  
  # Process diff_normal data
  diff_normal_daily <- diff_normal %>%
    group_by(year) %>%
    summarise(residual_GPP = mean(GPP_Anom_rf_diff, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(site_drought = site_drought, data_type = "non-legacy years")
  
  # Process diff_legacy data
  diff_legacy_daily <- diff_legacy %>%
    filter(year %in% legacy_years) %>%
    group_by(year, normal_year) %>%
    summarise(residual_GPP = mean(GPP_Anom_rf_diff, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(year) %>%
    summarise(
      median = median(residual_GPP, na.rm = TRUE),
      quan_5 = quantile(residual_GPP, probs = c(0.05), na.rm = TRUE),
      quan_95 = quantile(residual_GPP, probs = c(0.95), na.rm = TRUE)
    ) %>%
    mutate(site_drought = site_drought, data_type = "legacy years")
  
  # Store processed data for plotting
  data_for_plot[[i]] <- list(diff_normal_daily = diff_normal_daily, diff_legacy_daily = diff_legacy_daily)
  
  print(i)
}

# Combine all data into a single dataframe for plotting
diff_normal_combined <- bind_rows(lapply(data_for_plot, function(x) x$diff_normal_daily))
diff_legacy_combined <- bind_rows(lapply(data_for_plot, function(x) x$diff_legacy_daily))

# Define color pool
color_pool <- c('#993404', '#d95f0e', '#fe9929', '#fed98e', '#ffffd4')

jpeg(paste0(rootpath,'/legacy_vs_uncertainty_each_site_SPEI.jpg'),width = 2400,height = 1800,units = 'px', res=200)
# Create the plot
ggplot() +
  geom_boxplot(data = diff_normal_combined, 
               aes(x = factor(site_drought), y = residual_GPP, fill = data_type), 
               outlier.color = NA) +
  geom_point(data = diff_legacy_combined, 
             aes(x = factor(site_drought), y = median, color = factor(year)), 
             size = 3) +
  geom_linerange(data = diff_legacy_combined, 
                 aes(x = factor(site_drought), ymin = quan_5, ymax = quan_95, color = factor(year))) +
  #scale_color_manual(values = c(color_pool, 'grey')) +
  scale_fill_manual(values = c('grey')) +
  xlab('Sites') +
  ylab(expression(Residuals~of~GPP~(g[C]~m^{-2}~d^{-1}))) +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  guides(fill = guide_legend(title = ""),
         color = guide_legend(title = "Legacy Years")) +
  facet_wrap(~site_drought, scales = "free")
dev.off()

