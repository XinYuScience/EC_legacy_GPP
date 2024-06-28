#Author: Xin Yu
#Email: xyu@bgc-jena.mpg.de
#The code is the self-defined functions to support calculation

#cumulative water deficit
computeWdefCum <- function(LE, precip) {
  
  n <- length(LE)
  ET <- LE / 2.45E+6    # J m-2 ==> kg m-2 (==mm)
  wdefCum <- rep(NA, n)
  wdefCum[1]  <- 0
  
  for (i in 2:n) {
    wdefCum[i] <- min(wdefCum[i-1]+precip[i]-ET[i],0)
    if (is.na(wdefCum[i])) {wdefCum[i] <- wdefCum[i-1] }
  } 
  
  wdefCum
  
}

#water availability index
simpleWAI_ET  <- function(precip=NULL, LE=NULL, awc=awc,spin_up=FALSE) {
  
  #spin up
  n <- length(precip)
  ET <- LE / 2.45E+6    # J m-2 ==> kg m-2 (==mm)
  
  m<-365*5
  WAI_0 <- rep(NA, m)
  WAI_0[1]  <- awc*0.5
  P_0<-rep(precip[1:365],5)
  ET_0<-rep(ET[1:365],5)
  
  input_0  <- rep(NA,m)
  
  for (i in 2:m) {
    input_0[i] <- min(P_0[i], awc - WAI_0[i-1])
    WAI_0[i] <- max(WAI_0[i-1] + input_0[i] - ET_0[i],0)
    
    if (is.na(WAI_0[i])) {WAI_0[i] <- WAI_0[i-1] }
    
  } 
  
  #run after spin-up
  WAI <- rep(NA, n)
  if(spin_up){
    WAI[1]  <- WAI_0[length(WAI_0)] # set the initial value as the last value from the warm-up
  }else{
    WAI[1]<-awc*0.6
  }
  
  input  <- rep(NA, n)
  
  for (i in 2:n) {
    input[i] <- min(precip[i], awc - WAI[i-1])
    WAI[i] <- max(WAI[i-1] + input[i] - ET[i],0)
    if (is.na(WAI[i])) {WAI[i] <- WAI[i-1] }
  } 
  
  data.frame(WAI=WAI, WAIinput=input)
}

#quantify legacy effects using non-QC data but training the random forest model based on QC data
quantify_legacy_effects_non_QC<-function(data=data,figures_folder=figures_folder,site=site,
                                  vars=vars,EVI_flag=EVI_flag){
  #browser()
  #select training data
  training_data<-data %>% filter(group==0)
  legacy_data<-data %>% filter(group==1)
  
  if(EVI_flag==1){
    vars<-c(vars,'EVI')
  }
  
  #Random Forest
  library(randomForest)
  # for reproduciblity
  set.seed(123)
  vars<-paste(vars,'_Anom',sep='')
  train<-training_data %>% dplyr::select(vars,'doy')
  test<-train %>% drop_na()
  legacy_data<-legacy_data %>% select(-c(GPP_Anom,SW_IN_Anom,TA_Anom,VPD_Anom)) %>% rename(
    GPP_Anom=GPP_non_QC_Anom,
    SW_IN_Anom=SW_IN_non_QC_Anom,
    TA_Anom=TA_non_QC_Anom,
    VPD_Anom=VPD_non_QC_Anom
    )
  if(length(test$GPP_Anom)==0){
    var_explained<-NA
    
    legacy_data$GPP_Anom_rf<-rep(NA,length(legacy_data$Date))
    legacy_data$GPP_Anom_rf_diff<-rep(NA,length(legacy_data$Date))
  }else{
    rf<-randomForest(formula = GPP_Anom ~ .,data = train,ntree=400,na.action = na.exclude,
                     importance=T,mtry=4,nodesize=5)
    
    var_explained<-rf$rsq[length(rf$rsq)]
    
    legacy_data$GPP_Anom_rf<-predict(rf,newdata = legacy_data)
    legacy_data$GPP_Anom_rf_diff<-legacy_data$GPP_Anom-legacy_data$GPP_Anom_rf
  }
  
  #return diff
  legacy<-legacy_data %>% dplyr::select(Date,year,week,doy,GPP_Anom,GPP_Anom_rf,GPP_Anom_rf_diff)
  #browser()
  diff<-list(legacy=legacy,var_explained=var_explained)
  return(diff)
}

#quantify legacy effects considering uncertainty using non-QC data but training the random forest model based on QC data
quantify_legacy_effects_non_QC_uncertainty<-function(data=data,random_normal_year=random_normal_year,
                                     figures_folder=figures_folder,site=site,
                                     vars=vars,EVI_flag=EVI_flag){
  #select training data
  training_data<-data %>% filter(group==0 & !(year %in% random_normal_year))
  legacy_data<-data %>% filter(group==1)
  random_normal_data<-data %>% filter(group==0 & (year %in% random_normal_year))

  if(EVI_flag==1){
    vars<-c(vars,'EVI')
  }
  
  #Random Forest
  library(randomForest)
  set.seed(123)
  vars<-paste(vars,'_Anom',sep='')
  train<-training_data %>% dplyr::select(vars,'doy')
  test<-train %>% drop_na()
  legacy_data<-legacy_data %>% select(-c(GPP_Anom,SW_IN_Anom,TA_Anom,VPD_Anom)) %>% rename(
    GPP_Anom=GPP_non_QC_Anom,
    SW_IN_Anom=SW_IN_non_QC_Anom,
    TA_Anom=TA_non_QC_Anom,
    VPD_Anom=VPD_non_QC_Anom
  )
  if(EVI_flag==1){
    legacy_data<-legacy_data %>% select(-c(EVI_Anom)) %>% rename(
      EVI_Anom=EVI_non_QC_Anom
    )
  }
  if(length(test$GPP_Anom)==0){
    var_importance<-NA
    var_explained<-NA
    #residuals calculation
    random_normal_data$GPP_Anom_rf<-rep(NA,length(random_normal_data$Date))
    random_normal_data$GPP_Anom_rf_diff<-rep(NA,length(random_normal_data$Date))
    
    legacy_data$GPP_Anom_rf<-rep(NA,length(legacy_data$Date))
    legacy_data$GPP_Anom_rf_diff<-rep(NA,length(legacy_data$Date))
    legacy_data$normal_year<-rep(random_normal_year,length(legacy_data$Date))
  }else{
    rf<-randomForest(formula = GPP_Anom ~ .,data = train,ntree=400,na.action = na.exclude,
                     importance=T,mtry=4,nodesize=5)
    
    
    var_explained<-rf$rsq[length(rf$rsq)]
    
    #residuals calculation
    random_normal_data$GPP_Anom_rf<-predict(rf,newdata = random_normal_data)
    random_normal_data$GPP_Anom_rf_diff<-random_normal_data$GPP_Anom-random_normal_data$GPP_Anom_rf
    
    legacy_data$GPP_Anom_rf<-predict(rf,newdata = legacy_data)
    legacy_data$GPP_Anom_rf_diff<-legacy_data$GPP_Anom-legacy_data$GPP_Anom_rf
    legacy_data$normal_year<-rep(random_normal_year,length(legacy_data$Date))
    #browser()
  }
 
  #return diff
  normal<-random_normal_data %>% dplyr::select(Date,year,week,doy,GPP_Anom,GPP_Anom_rf,GPP_Anom_rf_diff)
  legacy<-legacy_data %>% dplyr::select(Date,year,week,doy,GPP_Anom,GPP_Anom_rf,GPP_Anom_rf_diff,normal_year)
 
  diff<-list(normal=normal,legacy=legacy,var_explained=var_explained)
  return(diff)
}

#quality control
quality_control<-function(data=data,vars=vars,site=site,flag=0.7,threshold=0){
  #browser()
  for (i in 1:length(vars)){
    if (vars[i] %in% colnames(data)){
        qc_var<-paste(vars[i],'_QC',sep = '')
        var_non_QC<-paste(vars[i],'_non_QC',sep = '')
        data[[var_non_QC]]<-data[[vars[i]]]
        data[[vars[i]]][data[[qc_var]]<flag]<-NA
        data[[vars[i]]][data[[vars[i]]]==-9999]<-NA
        if(vars[i] == 'GPP'){
          data[[vars[i]]][data[[vars[i]]]<threshold]<-NA
          data[[var_non_QC]][data[[var_non_QC]]<threshold]<-NA
        }
    }
  }
  return(data)
}

#growing season filter
gs_filter<-function(data=data,vars=vars,quan_GPP=0.25){
  
  #browser()
  #define growing season
  mean_GPP<-data %>% 
    dplyr::select(doy,GPP) %>%
    dplyr::group_by(doy) %>%
    summarize(seasonal = mean(GPP,na.rm = T))
  #calculate seasonal cycle
  
  span<-7
  fit <- with(mean_GPP,
              ksmooth(doy,seasonal, kernel = "normal", bandwidth = span))
  mean_GPP$seasonal_smooth<-fit$y
  mean_GPP_GS<-mean_GPP %>% filter(mean_GPP$seasonal_smooth>=quan_GPP*max(mean_GPP$seasonal_smooth,na.rm = T))
  doy_GS<-mean_GPP_GS$doy # doy in growing season
  
  # data filter of growing season
  for (i in 1:length(vars)){
    if (vars[i] %in% colnames(data)){
      data[[vars[i]]][!(data$doy %in% doy_GS)]<-NA
    }
  }
  data$doy_GS<-ifelse(data$doy %in% doy_GS,1,0)
  
  return(data)
}

#create DateTime, doy, month, year columns and order
create_doy<-function(data=data){
  # browser()
  if('TIMESTAMP' %in% colnames(data)){
    data$Date<- ymd(data$TIMESTAMP)
  }
  data$Date<-as.Date(data$Date)
  #browser()
  data<- mutate(data,
    doy=yday(Date),
    week=week(Date),
    month=month(Date),
    year=year(Date)
  ) 
  
  data<-select(data,Date,year,month,week,doy,everything())
  return(data)
}

#create DateTime, doy, month, year columns and order for AU-ASM in OzFlux
create_doy_OzFlux<-function(data=data){
  # browser()
  
  data$Date<-as.Date(data$Date,'%m/%d/%Y')
  data<- mutate(data,
                doy=yday(Date),
                week=week(Date),
                month=month(Date),
                year=year(Date)
  ) 
  
  data<-select(data,Date,year,month,week,doy,everything())
  return(data)
}

#calculate anomalies of all variables
anomaly_all<-function(data=data,vars=vars
                      ,site=site,figures_folder=figures_folder,sd=sd){
  for (i in 1:length(vars)){
    # browser()
    if (vars[i] %in% colnames(data)){
      data<-anomaly(data=data,var = vars[i],vars=vars,site = site,sd=sd)
    }
  }
  return(data)
}
#calculate anomalies of all variables
anomaly_non_QC_all<-function(data=data,vars=vars
                      ,site=site,figures_folder=figures_folder,sd=sd){
  for (i in 1:length(vars)){
    # browser()
    if (vars[i] %in% colnames(data)){
      data<-anomaly_non_QC(data=data,var = vars[i],vars=vars,site = site,sd=sd)
    }
  }
  return(data)
}

#calculate anomaly
anomaly<-function(data = df,var='GPP',vars=vars,site = site,sd=sd){
  #long-term linear trend
  mk<-MannKendall(data[[var]])
  if (mk$sl[1]<=0.05){
    data<-data %>% mutate(id = row_number())
    linearMod <- lm(get(var) ~ id, data=data,na.action=na.exclude)
    data[[paste(var,'_detrend', sep="")]]<-data[[var]]-(data$id*linearMod$coefficients[2][[1]]+linearMod$coefficients[1][[1]])
  }else{
    data[[paste(var,'_detrend', sep="")]]<-data[[var]]
  }
  
  #calculate seasonal cycle
  mean_data<-data %>% group_by(doy) %>%
    summarize(seasonal = mean(get(paste(var,'_detrend', sep="")),na.rm = T))

  
  #add mean seasonal cycle to existing data using matching value 'doy'
  data[[paste(var,'_detrend_seasonal', sep="")]]<- mean_data$seasonal[match(data$doy,mean_data$doy)]
  
  #calculate anomaly
  data[[paste(var,'_Anom', sep="")]]<-data[[paste(var,'_detrend', sep="")]]-data[[paste(var,'_detrend_seasonal', sep="")]]

 
  
  sd_threshold<-sd
 
  SD_universal<-sd(data[[paste(var,'_Anom', sep="")]],na.rm = T)
  
  data[[paste(var,'_index', sep="")]]<-ifelse(data[[paste(var,'_Anom', sep="")]]
                                              <=-sd_threshold*SD_universal,
                                              1,ifelse(data[[paste(var,'_Anom', sep="")]]
                                                       >=sd_threshold*SD_universal,2,0))
  
  return(data)
}
#calculate anomaly for non-QC data
anomaly_non_QC<-function(data = df,var='GPP',vars=vars,site = site,sd=sd){
  var_non_QC<-paste0(var,'_non_QC')
  #long-term linear trend
  mk<-MannKendall(data[[var]])
  if (mk$sl[1]<=0.05){
    data<-data %>% mutate(id = row_number())
    linearMod <- lm(get(var) ~ id, data=data,na.action=na.exclude)
    data[[paste(var,'_detrend', sep="")]]<-data[[var]]-(data$id*linearMod$coefficients[2][[1]]+linearMod$coefficients[1][[1]])
    data[[paste(var_non_QC,'_detrend', sep="")]]<-data[[var_non_QC]]-(data$id*linearMod$coefficients[2][[1]]+linearMod$coefficients[1][[1]])
  }else{
    data[[paste(var,'_detrend', sep="")]]<-data[[var]]
    data[[paste(var_non_QC,'_detrend', sep="")]]<-data[[var_non_QC]]
  }
  
  #calculate seasonal cycle
  mean_data<-data %>% group_by(doy) %>%
    summarize(seasonal = mean(get(paste(var,'_detrend', sep="")),na.rm = T))
  
  #add mean seasonal cycle to existing data using matching value 'doy'
  data[[paste(var,'_detrend_seasonal', sep="")]]<- mean_data$seasonal[match(data$doy,mean_data$doy)]
  
  #calculate anomaly
  data[[paste(var,'_Anom', sep="")]]<-data[[paste(var,'_detrend', sep="")]]-data[[paste(var,'_detrend_seasonal', sep="")]]
  data[[paste(var_non_QC,'_Anom', sep="")]]<-data[[paste(var_non_QC,'_detrend', sep="")]]-data[[paste(var,'_detrend_seasonal', sep="")]]
  
  sd_threshold<-sd
  
  SD_universal<-sd(data[[paste(var,'_Anom', sep="")]],na.rm = T)
  
  data[[paste(var,'_index', sep="")]]<-ifelse(data[[paste(var,'_Anom', sep="")]]
                                              <=-sd_threshold*SD_universal,
                                              1,ifelse(data[[paste(var,'_Anom', sep="")]]
                                                       >=sd_threshold*SD_universal,2,0))
  
  return(data)
}
