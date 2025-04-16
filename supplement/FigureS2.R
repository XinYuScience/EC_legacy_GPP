library(dyplr)
library(ggplot2)
rootpath<-'/Net/Groups/BGI/people/xyu/legacy_EC/2nd_writing_SA/response_code/'
data<-read.csv(paste0(rootpath,'drought_events_list_legacy_11.0_spin_up_00_OzFlux_weekly_gap_QC_mix_duration.csv'))
SPEI_flag<-read.csv(paste0(rootpath,'droughts_SPEI.csv'))
data$SPEI_flag<-SPEI_flag$SPEI_1_check_lag[match(paste0(data$site,data$drought_year),paste0(SPEI_flag$site,SPEI_flag$drought_year))]
data$legacy<-ifelse(data$SPEI_flag==1,data$legacy,NA)
data$concurrent<-ifelse(data$SPEI_flag==1,data$concurrent,NA)


data$group<-ifelse(is.na(data$legacy),'no drought',ifelse(data$legacy>0.00,'positive',ifelse(data$legacy<(-0.00),'negative','no legacy effects')))

df_legacy<-data %>% filter(group %in% c('positive','negative')) %>% 
  select(site,drought_year,legacy_years_length,no_record,legacy) %>% tidyr::drop_na() %>% filter(legacy!=0)

for (i in 1:nrow(df_legacy)) {
  site <- df_legacy$site[i]
  drought_year <- df_legacy$drought_year[i]
  site_drought<-paste0(site,' ',drought_year)
  legacy_years <- seq(from = df_legacy$drought_year[i], 
                      by = 1, 
                      length.out = df_legacy$legacy_years_length[i]+1)
  
  legacy_weekly <- read.csv(paste0(rootpath, '/legacy_weekly_sig_recovery_boxplot/', site, '_sig_1.csv')) %>% filter(year %in% legacy_years)
  legacy_weekly$residual_GPP<-legacy_weekly$residual_GPP/7
  
  na_groups <- legacy_weekly %>%
    group_by(year) %>%
    summarise(all_na = all(is.na(residual_GPP))) %>% filter(all_na==TRUE)
  
  stat.test_one <- legacy_weekly %>%
    group_by(year) %>% mutate(residual_GPP = ifelse(year %in% na_groups$year, 0, residual_GPP)) %>%
    wilcox_test(residual_GPP~1,mu=0) %>% 
    add_significance() %>%
    add_xy_position(x = "year")
  
  # Add a column to distinguish subplots
  legacy_weekly$site_drought <- site_drought
  stat.test_one$site_drought <- site_drought
  
  # Store data and stat.test results into a list
  if (i == 1) {
    combined_data <- legacy_weekly
    combined_stat <- stat.test_one
  } else {
    combined_data <- bind_rows(combined_data, legacy_weekly)
    combined_stat <- bind_rows(combined_stat, stat.test_one)
  }
  print(i)
}
jpeg(paste0(rootpath,'/legacy_sig_each_site_SPEI.jpg'),width = 2400,height = 2400,units = 'px', res=200)
# Create a ggplot with facet_wrap to display all subplots
ggplot(combined_data, aes(x = factor(year), y = residual_GPP)) +
  geom_boxplot(fill = 'white', color = "skyblue") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red") +
  stat_pvalue_manual(combined_stat, label = "p.signif", tip.length = 0.01,hide.ns = F) +
  ylab(expression(Residuals~of~GPP~(g[C]~m^{-2}~d^{-1}))) +
  xlab('') +
  theme_classic() +
  coord_cartesian(clip = "off") +  # Prevent clipping of significance symbols
  facet_wrap(~site_drought, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme(strip.text = element_text(size = 8),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))+
  NULL
dev.off()