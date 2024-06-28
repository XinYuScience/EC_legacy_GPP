#Author: Xin Yu
#Email: xyu@bgc-jena.mpg.de
#The code is to develop a support vector regression model to evaluate the relationship 
#between legacy effects in forests and their drivers, i.e. species richness and wood density variability
library(e1071)
library(iml)
data<-read.csv('') %>% 
  filter(PFT %in% c('EBF','ENF','DBF','MF')) %>%
  filter(no_record==0) %>% 
  filter(!(site=='DE-Tha' & drought_year == 2006)) %>%
  filter(legacy!=0) %>%
  select(legacy,species_richness,sd_wood_density) %>%
  select(everything()) %>% drop_na()

data$legacy<-data$legacy*100

svm_model<-svm(legacy~species_richness+sd_wood_density,data = data,kernel='radial')
predictions <- predict(svm_model, data=data)
data$legacy_hat<-predictions
R2_lm<-lm(legacy_hat~legacy,data=data)
summary(R2_lm)$r.squared

# Extracting coefficients
intercept <- coef(R2_lm)[1]

slope <- coef(R2_lm)[2]

figure_file<-'.tiff'
tiff(figure_file,width = 1400,height = 1400,units = 'px', res=400)
ggplot(data, aes(x=legacy,y=legacy_hat)) +
  geom_point(color = '#cbd5e8', size = 3,shape =1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(-10, 10)) +
  scale_y_continuous(limits = c(-10, 10)) +
  labs(x = "Observed legacy effects (%)", y = "Predicted legacy effects (%)") +
  geom_text(x = 8, y = 8, label = "1:1 line", angle = 45,hjust = 0, vjust = 1, color = "grey") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(size = 0.5),
        plot.title = element_text(hjust = 0.5))+
  annotate("text", x = -5, y = 8, label = expression('R'^'2'~'='~'0.59'), size = 5) +
  coord_equal(clip='off')
dev.off()