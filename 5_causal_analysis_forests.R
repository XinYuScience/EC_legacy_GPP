#Author: Xin Yu
#Email: xyu@bgc-jena.mpg.de
#The code is to identify the drivers of legacy effects for forests using HSIC perm, HSIC gamma, and KCIT
library(RCIT)
library(kpcalg)
library(energy)
library(dplyr)
library(poolr)
library(jmuOutlier)

data<-read.csv('') %>%
  filter(PFT %in% c('EBF','ENF','MF','DBF')) %>%
  filter(no_record==0) %>% 
  filter(!(site=='DE-Tha' & drought_year == 2006)) %>%
  filter(legacy!=0) %>%
  dplyr::select(-c(site,drought_year,legacy_years_length,
            no_record)) 

data<-data %>% dplyr::select(-PFT)

legacy<-'legacy'
names<-colnames(data)

p<-rep(0,length(names))
p2<-rep(0,length(names))
p3<-rep(0,length(names))
for (i in 1:length(names)) {
  temp<-data %>% dplyr::select(!!legacy,names[i]) %>% tidyr::drop_na() %>% filter(is.finite(get(names[i])))
  temp<-as.data.frame(sapply(temp, function(data) (data-mean(data,na.rm = T))/sd(data,na.rm = T)))
  p[i]<-U_KCI(temp[[names[i]]],temp[[legacy]])
  p2[i]<-hsic.gamma(temp[[names[i]]],temp[[legacy]],sig = 1)$p.value
  p3[i]<-hsic.perm(temp[[names[i]]],temp[[legacy]],sig = 1)$p.value
}

dt_temp<-data.frame(factor=names,kcit=p,hsic_gamma=p2,hsic_perm=p3)

write.csv(dt_temp,'.csv'),row.names = F)
