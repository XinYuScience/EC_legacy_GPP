#Author: Xin Yu
#Email: xyu@bgc-jena.mpg.de
cluster<-T
if(cluster){
  rootpath<-'/Net/Groups/BGI/people/xyu/legacy_EC'
  datapath<-'/Net/Groups/BGI/people/xyu'
  scratch_path<-'/Net/Groups/BGI/scratch/xyu'
}else{
  rootpath<-'X:/legacy_EC'
  scratch_path<-'Z:/scratch/xyu'
}

#source functions code
source('code_2.0/Functions.R')

#load packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringi)
library(Kendall)
library(zoo)
library(rstatix)
library(ggpubr)

sd<-1
tolerance_length<-3
drought_length<-20
case_run<-paste0('spin_up_00_OzFlux_weekly_gap_QC_mix_duration_',drought_length,'days')

site_info<-read.csv('all_sites_20240103.csv')
site_list<-site_info$site
# TP_ERA5<-read.csv(paste0(rootpath,'/2nd_study/all_sites_tp_20240521.csv'))

for (p in 1:length(site_list)) {
  site<-site_info$site[p]
  
  tryCatch({
    
############## read data ###############
    if(site_info$dataset[p]=='AmeriFlux_release'){
      folder<-paste0(datapath,'/AmeriFLUX_v1/AmeriFLUX_release_20220623')
      DD_file<-list.files(full.names = FALSE, path = folder,pattern = paste0(site_list[p],'_FLUXNET_FULLSET_DD')) # daily data's filename
      df<-read.csv(glue::glue('{folder}/{DD_file}'),na.strings = -9999) # read daily data
      df<-create_doy(data=df)
      #rename
      df<-df %>% rename(
        GPP=GPP_NT_VUT_USTAR50,
        RECO=RECO_NT_VUT_USTAR50,
        SW_IN=SW_IN_F_MDS,
        TA=TA_F_MDS,
        VPD=VPD_F_MDS,
        LE=LE_F_MDS,
        H=H_F_MDS,
        GPP_QC=NEE_VUT_REF_QC,
        SW_IN_QC=SW_IN_F_MDS_QC,
        TA_QC=TA_F_MDS_QC,
        VPD_QC=VPD_F_MDS_QC,
        LE_QC=LE_F_MDS_QC,
        H_QC=H_F_MDS_QC,
      ) %>% mutate(
        RECO_QC=GPP_QC
      )
      
    }else if(site_info$dataset[p] %in% c("FLUXNET 2015")){
      folder<-paste0('/Net/Groups/BGI/people/xyu/FLUXNET2015')
      DD_file<-list.files(full.names = FALSE, path = folder,pattern = paste0(site_list[p],'_FLUXNET2015_FULLSET_DD')) # daily data's filename
      df<-read.csv(glue::glue('{folder}/{DD_file}'),na.strings = -9999) # read daily data
      df<-create_doy(data=df)
      #rename
      df<-df %>% rename(
        GPP=GPP_NT_VUT_USTAR50,
        RECO=RECO_NT_VUT_USTAR50,
        SW_IN=SW_IN_F_MDS,
        TA=TA_F_MDS,
        VPD=VPD_F_MDS,
        LE=LE_F_MDS,
        H=H_F_MDS,
        GPP_QC=NEE_VUT_REF_QC,
        SW_IN_QC=SW_IN_F_MDS_QC,
        TA_QC=TA_F_MDS_QC,
        VPD_QC=VPD_F_MDS_QC,
        LE_QC=LE_F_MDS_QC,
        H_QC=H_F_MDS_QC,
      ) %>% mutate(
        RECO_QC=GPP_QC
      )
    }else if(site_info$dataset[p]=="ICOS Warm Winter 2020"){
      Datapath<-paste0('/Net/Groups/BGI/people/xyu/ICOS_EC')
      folders<-list.dirs(path = Datapath,full.names = TRUE, recursive = TRUE)
      target_folder<-grep(site_list[p], folders, value = TRUE) 
      DD_file<-list.files(full.names = FALSE, path = target_folder,pattern = 'FULLSET_DD') # daily data's filename
      
      #read data
      df<-read.csv(glue::glue('{target_folder}/{DD_file}'),na.strings = -9999) # read daily data
      df<-create_doy(data=df)
      #rename
      df<-df %>% rename(
        GPP=GPP_NT_VUT_USTAR50,
        RECO=RECO_NT_VUT_USTAR50,
        SW_IN=SW_IN_F_MDS,
        TA=TA_F_MDS,
        VPD=VPD_F_MDS,
        LE=LE_F_MDS,
        H=H_F_MDS,
        GPP_QC=NEE_VUT_REF_QC,
        SW_IN_QC=SW_IN_F_MDS_QC,
        TA_QC=TA_F_MDS_QC,
        VPD_QC=VPD_F_MDS_QC,
        LE_QC=LE_F_MDS_QC,
        H_QC=H_F_MDS_QC
      ) %>% mutate(
        RECO_QC=GPP_QC
      )
    }else if(site_info$dataset[p]=="OzFlux"){
      folder<-paste0(scratch_path,'/OzFlux/DD_csv_QC_v2')
      DD_file<-paste0(site,'.csv')
      df<-read.csv(glue::glue('{folder}/{DD_file}'),na.strings = -9999) # read daily data
      if(site=='AU-ASM'){
        df<-create_doy_OzFlux(data=df)
      }else{
        df<-create_doy(data=df)
      }
      #rename
      df<-df %>% rename(
        GPP=GPP_LT,
        RECO=ER_LT,
        SW_IN=Fsd,
        TA=Ta,
        VPD=VPD,
        P=Precip,
        LE=Fe,
        H=Fh,
        GPP_QC=GPP_LT_QCFlag,
        RECO_QC=ER_LT_QCFlag,
        SW_IN_QC=Fsd_QCFlag,
        TA_QC=Ta_QCFlag,
        VPD_QC=VPD_QCFlag,
        LE_QC=Fe_QCFlag,
        H_QC=Fh_QCFlag,
      )
      df$P_F<-as.numeric(df$P)
    }
    # ggplot(df)+
    #   geom_line(aes(doy,SW_IN))+
    #   facet_wrap(~year)
    # if(site_info$dataset[p]=="OzFlux"){
    #   TP_ERA5$date<-as.Date(TP_ERA5$X,format = "%Y-%m-%d")
    #   site_split<-strsplit(site,split = '-')[[1]]
    #   site_col<-paste0(site_split[1],'.',site_split[2])
    #   df$P_ERA5<-TP_ERA5[[site_col]][match(as.Date(df$Date),as.Date(TP_ERA5$X))]
    #   df<-df %>% mutate(
    #     P_F = ifelse(is.na(P),P_ERA5,P)
    #   )
    # }
    # ggplot(df)+
    #   geom_line(aes(doy,GPP))+
    #   facet_wrap(~year)
    if(site_info$dataset[p]=="OzFlux"){
      df$Date_pot<-as.Date(df$Date)-181
      df<- mutate(df,
                  doy_pot=yday(Date_pot),
                  week_pot=week(Date_pot),
                  month_pot=month(Date_pot),
                  year_pot=year(Date_pot)
      )
      df<-df %>% rename(
        Date_org=Date,
        year_org=year,
        month_org=month,
        week_org=week,
        doy_org=doy,
        Date=Date_pot,
        year=year_pot,
        month=month_pot,
        week=week_pot,
        doy=doy_pot
      )
    }
    
    
    # # ggplot(df)+
    # #   geom_line(aes(doy,GPP))+
    # #   facet_wrap(~year)
    # # Function to find repeated 3-element sequences
    # find_repeated_sequences <- function(seq) {
    #   # Initialize a list to store sequences and their start indices
    #   sequences <- list()
    #   
    #   # Iterate over the sequence
    #   for(i in 1:(length(seq)-2)) {
    #     # Current 3-element sequence
    #     current_seq <- paste(seq[i], seq[i+1], seq[i+2], sep = ",")
    #     
    #     # Check if this sequence is already in the list
    #     if(current_seq %in% names(sequences)) {
    #       # Append the current index to the existing entry
    #       sequences[[current_seq]] <- c(sequences[[current_seq]], i)
    #     } else {
    #       # Create a new entry for this sequence
    #       sequences[[current_seq]] <- i
    #     }
    #   }
    #   
    #   # Filter sequences that occur more than once
    #   repeated_sequences <- sequences[sapply(sequences, length) > 1]
    #   
    #   return(repeated_sequences)
    # }
    # # Find repeated sequences
    # repeated_sequences <- find_repeated_sequences(df$LE)
    # 
    # # Initialize a flag column with all zeros (no sequence found)
    # df$Flag <- 0
    # 
    # # Function to flag time steps
    # flag_time_steps <- function(df, sequences) {
    #   for(seq in names(sequences)) {
    #     for(start_index in sequences[[seq]]) {
    #       # Flag the current sequence and the next two elements
    #       df$Flag[start_index:(start_index+2)] <- 1
    #     }
    #   }
    #   return(df)
    # }
    # 
    # # Flag the time steps
    # df <- flag_time_steps(df, repeated_sequences)
    # 
    # 
    # #Calculate the fraction of flags per year
    # yearly_flags <- df %>%
    #   group_by(year) %>%
    #   summarise(Total = n(),
    #             Flagged = sum(Flag),
    #             FractionFlagged = Flagged / Total)
    # 
    # # Step 2: Filter out years with >30% flags
    # filtered_years <- yearly_flags %>%
    #   filter(FractionFlagged <= 0.2) %>%
    #   pull(year) # Extracting the years for further processing
    # 
    # # Step 3: Select the longest continuous non-flagged period
    # get_longest_period <- function(years) {
    #   # Sort years and calculate differences
    #   years <- sort(years)
    #   diffs <- diff(years)
    #   
    #   # Identify breaks in continuity
    #   breaks <- which(diffs > 1)
    #   
    #   if(length(breaks) == 0) {
    #     return(list(start_year=min(years),end_year=max(years)))
    #   } else {
    #     # Calculate lengths of continuous periods
    #     lengths <- c(breaks[1], diff(breaks), length(years) - breaks[length(breaks)])
    #     
    #     # Identify the longest period
    #     longest <- which.max(lengths)
    #     
    #     # Return the start and end of the longest period
    #     start_year <- years[c(1, breaks + 1)[longest]]
    #     end_year <- years[c(breaks, length(years))[longest]]
    #     
    #     return(list(start_year=start_year,end_year=end_year))
    #   }
    #   
    # }
    # # Apply the function to get the longest period
    # longest_period <- get_longest_period(filtered_years)
    
    #variables
    vars<-c('GPP','RECO','SW_IN','TA','VPD','LE','H')
    vars_rf_GPP<-c('GPP','SW_IN','TA','VPD')
    vars_QC<-paste0(vars,'_QC')
    vars_time<-c('Date','doy','week','month','year')

    # df<-df %>% dplyr::select(vars_time,vars,vars_QC,'SW_IN_POT','P_F','LE_CORR')
    
    if(site_info$group[p] %in% c("fluxnet-BGI","ICOS")){
      WAI<-read.csv(file = paste0(rootpath,'/FluxNet-BGI_P_LE_WAI_ts/',site,'.csv'))
      # df$WAI2<-WAI$WAI[match(as.Date(df$Date),as.Date(WAI$Date))]
      min_year<-min(WAI$year)
      max_year<-max(WAI$year)
      df<-df %>% filter(year>=min_year & year<=max_year)
    }else if(site_info$group[p]=="AmeriFLUX"){
      WAI<-read.csv(file = paste0(rootpath,'/AmeriFLUX_P_LE_WAI_ts/',site,'.csv'))
      # df$WAI2<-WAI$WAI[match(as.Date(df$Date),as.Date(WAI$Date))]
      min_year<-min(WAI$year)
      max_year<-max(WAI$year)
      df<-df %>% filter(year>=min_year & year<=max_year)
    }else if(site_info$group[p]=="OzFlux"){
      WAI<-read.csv(file = paste0(rootpath,'/OzFlux_P_LE_WAI_ts/',site,'.csv'))
      # df$WAI2<-WAI$WAI[match(as.Date(df$Date),as.Date(WAI$Date))]
      # min_year<-min(WAI$year)
      # max_year<-max(WAI$year)
      df<-df #%>% filter(year>=min_year & year<=max_year)
    }
    # df<-df %>% filter(year>=longest_period$start_year & year<=longest_period$end_year)
    # # # Print the longest continuous non-flagged period
    # # print(longest_period)
    
    # ggplot(df)+
    #   geom_line(aes(doy,P_F))+
    #   facet_wrap(~year)
    # # Plot
    # tiff(paste0('LE_random_noise_flag/',site,'.tiff'))
    # plot_LE<-ggplot(df)+
    #   geom_vline(data=df %>% filter(year>=longest_period$start_year & year<=longest_period$end_year),
    #              aes(xintercept = doy),size=2,color='grey')+
    #   geom_line(aes(doy,LE,color=factor(Flag)))+
    #   facet_wrap(~year)
    # print(plot_LE)
    # dev.off()
    
 
    
    #water availability index
    if(site=='DE-Hai'){
      df$LEperday<-df$LE_CORR*3600*24
    }else{
      df$LEperday<-df$LE*3600*24
    }
    df$WdefCum<-computeWdefCum(df$LEperday,df$P_F) #Calculate the cumulative water deficit to infer the water bucket size
    awc<-abs(min(df$WdefCum,na.rm = T)) # set the maximum water deficit as the water bucket size
    WAI<-simpleWAI_ET(df$P_F,df$LEperday,awc=awc,spin_up=TRUE)
    # WAI_0.6<-simpleWAI_ET(df$P_F,df$LEperday,awc=awc,spin_up=FALSE)
    df$WAI<-WAI$WAI
    # df$WAI3<-WAI_0.6$WAI
    # folder_path_WAI<-paste0(rootpath,'/2nd_study/WAI_',case_run,'/')
    # if (!dir.exists(folder_path_WAI)) {
    #   dir.create(folder_path_WAI)
    # }
    # figure_file<-paste0(folder_path_WAI,site,'_WAI.tiff')
    # tiff(figure_file,width = 1600,height = 1200,units = 'px', res=200)
    # WAI_plot<-ggplot(df)+
    #   geom_line(aes(doy,WAI2,color='original'),size=2)+
    #   geom_line(aes(doy,WAI,color='spin-up'))+
    #   geom_line(aes(doy,WAI2,color='0.6_bucket_size'))+
    #   facet_wrap(~year)+
    #   ylab('WAI')
    # print(WAI_plot)
    # dev.off()
    
    # plot(df$WAI,df$WAI2)
    
    #plot(df$P_F,WAI$P_F)
    ggplot(df)+
      geom_line(aes(doy,WAI))+
      #geom_line(aes(doy,WAI2),color='red')+
      facet_wrap(~year)
    # ggplot(df)+
    #   geom_line(aes(doy,GPP))+
    #   facet_wrap(~year)
    # calculate evaporative fraction
    df<-df %>% mutate(EF=LE/(LE+H))
    df$EF[df$EF<0]<-NA #remove unphysical values
    df$EF[df$EF>1]<-NA
    
    if(site=='DE-Lnf'){
      df$EF<-ifelse(df$year %in% c('2007','2008','2009'),NA,df$EF)
    }
    
    #quality control
    # if(grepl("non_QC", case_run)){
    #   df_QC<-df
    # }else{
    #   df_QC<-quality_control(data = df,site = site,vars = vars,threshold = 0)
    # }
    df_QC<-quality_control(data = df,site = site,vars = vars,threshold = 0)
    
    ggplot(df_QC)+
      geom_point(aes(doy,GPP_non_QC,color='GPP_non_QC'))+
      geom_point(aes(doy,GPP,color='GPP'))+
      facet_wrap(~year)
    #growing season filter
    if(site %in% c('US-Var','US-Wkg')){
      quan_GPP<-0.1
    }else{
      quan_GPP<-0.25
    }

    # #define growing season
    # mean_GPP<-df %>%
    #   dplyr::select(doy,GPP) %>%
    #   dplyr::group_by(doy) %>%
    #   summarize(seasonal = mean(GPP,na.rm = T))
    # #calculate seasonal cycle
    #
    # span<-7
    # fit <- with(mean_GPP,
    #             ksmooth(doy,seasonal, kernel = "normal", bandwidth = span))
    # mean_GPP$seasonal_smooth<-fit$y
    # mean_GPP_GS<-mean_GPP %>% filter(mean_GPP$seasonal_smooth>=quan_GPP*max(mean_GPP$seasonal_smooth,na.rm = T))
    # doy_GS<-mean_GPP_GS$doy # doy in growing season
    #
    #
    # # data filter of growing season
    # for (i in 1:length(vars)){
    #   if (vars[i] %in% colnames(data)){
    #     data[[vars[i]]][!(data$doy %in% doy_GS)]<-NA
    #   }
    # }
    vars_non_QC<-paste0(vars,'_non_QC')
    df_QC_GS<-gs_filter(data = df_QC,vars = c(vars,vars_non_QC,'WAI','EF'),quan_GPP = quan_GPP)

    ggplot(df_QC_GS)+
      geom_point(aes(doy,GPP_non_QC))+
      geom_point(aes(doy,GPP,color='QC'))+
      facet_wrap(~year)
    ################ legacy effects quantification ###################

    #anomalies
    if(grepl("smooth", case_run)){
      df_QC_GS<-anomaly_smooth_all(data = df_QC_GS,vars = c(vars,'WAI','EF'),site = site,sd=sd,span=span)
      df_QC_GS<-anomaly_non_QC_smooth_all(data = df_QC_GS,vars = c(vars),site = site,sd=sd,span=span)
    }else{
      df_QC_GS<-anomaly_all(data = df_QC_GS,vars = c(vars,'WAI','EF'),site = site,sd=sd)
      df_QC_GS<-anomaly_non_QC_all(data = df_QC_GS,vars = c(vars),site = site,sd=sd)
    }
    ggplot(df_QC_GS)+
      geom_point(aes(doy,GPP_non_QC_Anom,color='non_QC'))+
      geom_point(aes(doy,GPP_Anom))+
      facet_wrap(~year)
    ################## identify drought events ######################
    #EF anomalies <= -1 SD
    df_QC_GS$drought<-ifelse(is.na(df_QC_GS$EF_index),0,ifelse(df_QC_GS$EF_index==1,1,0))

    #1) fill gaps in the middle of a real drought
    for (i in 1:(length(df_QC_GS$doy)-tolerance_length)) {
      #print(i)
      if(df_QC_GS$drought[i]==0){
        next
      }else{
        if(df_QC_GS$drought[i+1]==0){
          if(sum(df_QC_GS$drought[(i+1):min(i+tolerance_length+1,length(df_QC_GS$Date))])==0){
            df_QC_GS$drought[i]<-0
          }else{
            for (j in 1:min(tolerance_length+1,length(df_QC_GS$Date)-i)) {
              if(df_QC_GS$drought[i+j]==1){
                df_QC_GS$drought[(i+1):(i+j)]<-1
                i<-i+j-1
                break
              }
            }
          }
        }
      }
    }

    #2) select relative long-term drought
    ones_indices<-which(df_QC_GS$drought == 1)
    consecutive_sequences<-split(ones_indices,cumsum(c(0,diff(ones_indices)!=1)))
    short_sequences <- Filter(function(x) length(x) <= drought_length, consecutive_sequences)
    # Set 1 to 0 for the identified sequences
    for (seq_indices in short_sequences) {
      df_QC_GS$drought[seq_indices] <- 0
    }
    # Find consecutive sequences longer than the threshold
    long_sequences <- Filter(function(x) length(x) > drought_length, consecutive_sequences)



    #3) select the 1st and 2nd strongest droughts based on cumulative drought intensity (i.e. cumulative EF anomalies)
    df_QC_GS$drought_events<-0
    drought_events_num<-1
    for (i in 1:(length(df_QC_GS$Date)-1)) {
      if(df_QC_GS$drought[i]==1){
        df_QC_GS$drought_events[i]<-drought_events_num
        if(df_QC_GS$drought[i+1]==0){
          drought_events_num<-drought_events_num+1
        }
      }
    }
    df_QC_GS$index <- seq.int(nrow(df_QC_GS))
    if(all(df_QC_GS$drought_events==0)){
      next
    }
    drought_events_array<-unique(df_QC_GS$drought_events)[2:length(unique(df_QC_GS$drought_events))]
    for (t in 1:length(drought_events_array)) {
      drought<-df_QC_GS %>% filter(drought_events==drought_events_array[t])
      WAI_drought<-drought$WAI
      if((drought$index[1]-length(WAI_drought))<=0){
        WAI_pre_drought<-df$WAI[1:(drought$index[1]-1)]
      }else{
        WAI_pre_drought<-df$WAI[(drought$index[1]-length(WAI_drought)):(drought$index[1]-1)]
      }
      t_test<-wilcox.test(WAI_drought,WAI_pre_drought)
      if(is.na(t_test$p.value)){
        df_QC_GS$drought_events[df_QC_GS$drought_events==t]<-0
      }else{
        if(t_test$p.value>0.05 | (mean(WAI_drought,na.rm = T)>mean(WAI_pre_drought,na.rm = T))){
          df_QC_GS$drought_events[df_QC_GS$drought_events==t]<-0
        }
      }
    }
    drought_cumu<-df_QC_GS %>% filter(drought_events>=1) %>% group_by(drought_events) %>%
      summarise(drought_cumu=sum(EF_Anom,na.rm = T),
                year=first(year))
    if(nrow(drought_cumu)>=2){
      if(length(unique(df_QC_GS$year))>14){
        drought_number<-2
      }else{
        drought_number<-1
      }
    }else if(nrow(drought_cumu)==1){
      drought_number<-1
    }else{
      drought_number<-0
    }

    droughts<-head(drought_cumu[order(drought_cumu$drought_cumu, decreasing = FALSE), ],drought_number)
    df_QC_GS$drought[!(df_QC_GS$drought_events %in% droughts$drought_events)]<-0
    drought_events_number<-droughts$drought_events[order(droughts$drought_events,decreasing = FALSE)]

    #################### quantify legacy periods #################
    df_QC_GS$legacy<-0
    no_record<-0
    no_record_2<-0

    if(drought_number==1){
      print('hello')


      temp<-which(df_QC_GS$drought_events==drought_events_number[1])
      drought_end<-temp[length(temp)]
      for (legacy_years_length in 1:(df_QC_GS$year[length(df_QC_GS$year)]-df_QC_GS$year[drought_end])) {
        print(paste0('drought legacy length:',legacy_years_length))
        df_QC_GS$legacy[which(df_QC_GS$year==df_QC_GS$year[drought_end] & df_QC_GS$doy>df_QC_GS$doy[drought_end])]<-1
        df_QC_GS$legacy[which(df_QC_GS$year>df_QC_GS$year[drought_end] &
                                df_QC_GS$year<=df_QC_GS$year[drought_end]+legacy_years_length)]<-1


        #flag legacy and non-legacy periods
        df_QC_GS$group<-0 #non-legacy
        df_QC_GS$group[df_QC_GS$legacy==1]<-1 #legacy
        temp_normal_year<-df_QC_GS %>% filter(legacy==1)
        normal_year<-unique(df_QC_GS$year)[!(unique(df_QC_GS$year) %in% unique(temp_normal_year$year))]


        ###### running the quantification algorithm ######
        diff_GPP<-quantify_legacy_effects_non_QC(data=df_QC_GS,figures_folder = figures_folder,site=site,vars = c(vars_rf_GPP,'WAI'),EVI_flag = 0)


        diff_legacy_GPP<-diff_GPP$legacy
        legacy<-diff_legacy_GPP %>% dplyr::select(year,doy,GPP_Anom_rf_diff) %>% rename(residual_GPP=GPP_Anom_rf_diff)
        legacy$year<-as.character(legacy$year)
        #print('shuai')
        #ggplot(diff_legacy_GPP)+geom_line(aes(doy,GPP_Anom_rf_diff))+facet_wrap(~year)
        # tiff(paste0(rootpath,'/2nd_study/sub_annual_convergence_figures/',site,'.tiff'),width = 1600,height = 1200,units = 'px', res=200)
        sub_plot<-diff_legacy_GPP %>% tidyr::pivot_longer(-c(Date,year,week,doy,GPP_Anom_rf_diff)) %>%
          ggplot(aes(doy,value,color=name))+
          geom_point()+
          stat_smooth(method = "loess", se = T,span = 0.3)+
          facet_wrap(~year)
        print(sub_plot)
        # dev.off()
        # break
      #   ggplot(diff_legacy_GPP)+geom_line(aes(doy,GPP_Anom,color='obs'))+geom_line(aes(doy,GPP_Anom_rf,color='rf'))+facet_wrap(~year)
        # aggregate daily to weekly
        legacy$week<-df$week[match(paste(legacy$year,legacy$doy),
                                   paste(df$year,df$doy))]
        legacy_weekly<-legacy %>% group_by(year,week) %>%
          summarise(residual_GPP=ifelse(n()>=4,mean(residual_GPP,na.rm=T)*7,NA))

        ggplot(legacy_weekly)+
          geom_line(aes(week,residual_GPP))+
          facet_wrap(~year)

        legacy_weekly<-legacy_weekly %>% select(year,residual_GPP)

        na_groups <- legacy_weekly %>%
          group_by(year) %>%
          summarise(all_na = all(is.na(residual_GPP))) %>% filter(all_na==TRUE)

        stat.test_one <- legacy_weekly %>%
          group_by(year) %>% mutate(residual_GPP = ifelse(year %in% na_groups$year, 0, residual_GPP)) %>%
          wilcox_test(residual_GPP~1,mu=0) %>% add_significance() %>%
          add_xy_position(x = "year")


        #test if convergent for the ONLY drought
        drought_year<-df_QC_GS$year[drought_end]
        if(legacy_years_length==1){
          if(length(stat.test_one$p[stat.test_one$year == drought_year+1])==0){
            no_record<-1
            break
          }else{
            if(stat.test_one$p[stat.test_one$year == drought_year+1]>0.05){
              break
            }
          }

        }else{
          if(stat.test_one$p[stat.test_one$year == drought_year+legacy_years_length]>0.05){
            break
          }else{
            if(stat.test_one$p[stat.test_one$year == drought_year+legacy_years_length-1]>0.05){
              legacy_years_length<-legacy_years_length-1
              break
            }
          }

          if(legacy_years_length == df_QC_GS$year[length(df_QC_GS$year)]-df_QC_GS$year[drought_end]){
            no_record<-1
          }
        }
      }



    }else if(drought_number==2){
      print('hello_2')

      #flag legacy and non-legacy periods
      legacy_years_length_1<-1
      legacy_years_length_2<-1
      legacy_years_length<-1
      temp_1<-which(df_QC_GS$drought_events==drought_events_number[1])
      drought_end_1<-temp_1[length(temp_1)]
      temp_2<-which(df_QC_GS$drought_events==drought_events_number[2])
      drought_end_2<-temp_2[length(temp_2)]
      maximum_length<-max(df_QC_GS$year[length(df_QC_GS$year)]-df_QC_GS$year[drought_end_2],df_QC_GS$year[drought_end_2]-df_QC_GS$year[drought_end_1]-1)
      finish<-FALSE
      finish_1<-FALSE
      finish_2<-FALSE
      while (finish==FALSE) {
        print(paste0('1st drought legacy length before:',legacy_years_length_1))
        print(paste0('2nd drought legacy length before:',legacy_years_length_2))

        #1st drought in order
        df_QC_GS$legacy[which(df_QC_GS$year==df_QC_GS$year[drought_end_1] & df_QC_GS$doy>df_QC_GS$doy[drought_end_1])]<-1
        df_QC_GS$legacy[which(df_QC_GS$year>df_QC_GS$year[drought_end_1] &
                                df_QC_GS$year<=df_QC_GS$year[drought_end_1]+legacy_years_length_1)]<-1

        #not identify the first drought but only keep the 2nd drought
        #if the 2nd drought occured in the legacy periods of the 1st drought
        temp_test<-df_QC_GS %>% filter(legacy==1) %>% filter(drought_events==drought_events_number[2])
        if(length(temp_test$Date)>0){
          df_QC_GS$legacy[temp_1[length(temp_1)]:(temp_2[length(temp_2)])]<-0 #keep the legacy periods of the 1st drought and reset the 2nd drought as non-legacy periods
          droughts<-droughts %>% filter(drought_events==drought_events_number[2])
          finish_1=TRUE
        }

        #2nd drought in order
        df_QC_GS$legacy[which(df_QC_GS$year==df_QC_GS$year[drought_end_2] & df_QC_GS$doy>df_QC_GS$doy[drought_end_2])]<-1
        df_QC_GS$legacy[which(df_QC_GS$year>df_QC_GS$year[drought_end_2] &
                                df_QC_GS$year<=df_QC_GS$year[drought_end_2]+legacy_years_length_2)]<-1

        #flag legacy and non-legacy periods
        df_QC_GS$group<-0 #non-legacy
        df_QC_GS$group[df_QC_GS$legacy==1]<-1 #legacy
        temp_normal_year<-df_QC_GS %>% filter(legacy==1)
        normal_year<-unique(df_QC_GS$year)[!(unique(df_QC_GS$year) %in% unique(temp_normal_year$year))]

        diff_GPP<-quantify_legacy_effects_non_QC(data=df_QC_GS,figures_folder = figures_folder,site=site,vars = c(vars_rf_GPP,'WAI'),EVI_flag = 0)
        var_ex_GPP<-diff_GPP$var_explained
        diff_legacy_GPP<-diff_GPP$legacy

        # tiff(paste0(rootpath,'/2nd_study/sub_annual_convergence_figures/',site,'.tiff'),width = 1600,height = 1200,units = 'px', res=200)
        sub_plot<-diff_legacy_GPP %>% tidyr::pivot_longer(-c(Date,year,week,doy,GPP_Anom_rf_diff)) %>%
          ggplot(aes(doy,value,color=name))+
          geom_line()+
          #stat_smooth(method = "loess", se = T,span = 0.3)+
          facet_wrap(~year)
        print(sub_plot)
        # dev.off()
        # finish<-TRUE
        legacy<-diff_legacy_GPP %>% dplyr::select(year,doy,GPP_Anom,GPP_Anom_rf,GPP_Anom_rf_diff) %>% rename(residual_GPP=GPP_Anom_rf_diff)
        legacy$year<-as.character(legacy$year)
        # aggregate daily to weekly
        legacy$week<-df$week[match(paste(legacy$year,legacy$doy),
                                   paste(df$year,df$doy))]
        legacy_weekly<-legacy %>% group_by(year,week) %>%
          summarise(residual_GPP=ifelse(n()>=4,mean(residual_GPP,na.rm=T)*7,NA))
        # legacy_weekly2<-legacy %>% group_by(year,week) %>%
        #   summarise(residual_GPP=mean(residual_GPP,na.rm=T)*7)
                    
        #legacy_weekly<-legacy_weekly %>% select(year,residual_GPP)
        legacy_weekly<-legacy_weekly %>% select(year,residual_GPP)
        
        na_groups <- legacy_weekly %>%
          group_by(year) %>%
          summarise(all_na = all(is.na(residual_GPP))) %>% filter(all_na==TRUE)

        stat.test_one <- legacy_weekly %>%
          group_by(year) %>% mutate(residual_GPP = ifelse(year %in% na_groups$year, 0, residual_GPP)) %>%
          wilcox_test(residual_GPP~1,mu=0) %>% 
          add_significance() %>%
          add_xy_position(x = "year")

        #test if covergent for the 1st drought
        if(finish_1==FALSE){
          drought_year_1<-df_QC_GS$year[drought_end_1]
          if(legacy_years_length_1==1){
            if(stat.test_one$p[stat.test_one$year == drought_year_1+1]>0.05){
              finish_1<-TRUE
            }else{
              legacy_years_length_1<-legacy_years_length_1+1
            }
          }else{
            if(stat.test_one$p[stat.test_one$year == drought_year_1+legacy_years_length_1]>0.05){
              finish_1<-TRUE
            }else{
              if(stat.test_one$p[stat.test_one$year == drought_year_1+legacy_years_length_1-1]>0.05){
                finish_1<-TRUE
                legacy_years_length_1<-legacy_years_length_1-1
              }else{
                legacy_years_length_1<-legacy_years_length_1+1
              }
            }
          }
        }
        #test if convergent for the 2nd drought
        if(finish_2==FALSE){
          drought_year_2<-df_QC_GS$year[drought_end_2]
          if(legacy_years_length_2==1){
            if(length(stat.test_one$p[stat.test_one$year == drought_year_2+1])==0){
              no_record_2<-1
              finish_2<-TRUE
            }else{
              if(stat.test_one$p[stat.test_one$year == drought_year_2+1]>0.05){
                finish_2<-TRUE
              }else{
                legacy_years_length_2<-legacy_years_length_2+1
              }
            }
          }else{
            if(stat.test_one$p[stat.test_one$year == drought_year_2+legacy_years_length_2]>0.05){
              finish_2<-TRUE
            }else{
              if(stat.test_one$p[stat.test_one$year == drought_year_2+legacy_years_length_2-1]>0.05){
                finish_2<-TRUE
                legacy_years_length_2<-legacy_years_length_2-1
              }else{
                legacy_years_length_2<-legacy_years_length_2+1
              }
            }
          }
        }
        if(legacy_years_length_2>df_QC_GS$year[length(df_QC_GS$year)]-df_QC_GS$year[drought_end_2]){
          no_record_2<-1
          finish_2=TRUE
        }
        legacy_years_length<-max(legacy_years_length_1,legacy_years_length_2)
        print(paste0('1st drought legacy length after:',legacy_years_length_1))
        print(paste0('2nd drought legacy length after:',legacy_years_length_2))
        if(finish_1==TRUE & finish_2==TRUE){
          finish=TRUE
        }

      }


    }else{
      next
    }

    stat<-stat.test_one %>% dplyr::select(year,p)
    folder_path_stat<-paste0(rootpath,'/2nd_study/stat_test_11.0_',case_run,'/')
    if (!dir.exists(folder_path_stat)) {
      dir.create(folder_path_stat)
    }
    write.csv(stat,file = paste0(folder_path_stat,site,'_stat_test_',sd,'.csv'),row.names = F)

    df_QC_GS$legacy<-0
    droughts$legacy_years_length<-0

    #quantify legacy effects again after identifying legacy periods
    if(drought_number==1){
      df_QC_GS$legacy[which(df_QC_GS$year==df_QC_GS$year[drought_end] & df_QC_GS$doy>df_QC_GS$doy[drought_end])]<-1
      df_QC_GS$legacy[which(df_QC_GS$year>df_QC_GS$year[drought_end] &
                              df_QC_GS$year<=df_QC_GS$year[drought_end]+legacy_years_length-1)]<-1
      droughts$legacy_years_length<-legacy_years_length
      droughts$no_record<-no_record
    }else{
      #1st drought in order
      df_QC_GS$legacy[which(df_QC_GS$year==df_QC_GS$year[drought_end_1] & df_QC_GS$doy>df_QC_GS$doy[drought_end_1])]<-1
      df_QC_GS$legacy[which(df_QC_GS$year>df_QC_GS$year[drought_end_1] &
                              df_QC_GS$year<=df_QC_GS$year[drought_end_1]+legacy_years_length_1-1)]<-1

      #not identify the first drought but only keep the 2nd drought
      #if the 2nd drought occured in the legacy periods of the 1st drought
      temp_test<-df_QC_GS %>% filter(legacy==1) %>% filter(drought_events==drought_events_number[2])
      if(length(temp_test$Date)>0){
        df_QC_GS$legacy[temp_2]<-0 #keep the legacy periods of the 1st drought and reset the 2nd drought as non-legacy periods
        finish_1=TRUE
      }

      #2nd drought in order
      df_QC_GS$legacy[which(df_QC_GS$year==df_QC_GS$year[drought_end_2] & df_QC_GS$doy>df_QC_GS$doy[drought_end_2])]<-1
      df_QC_GS$legacy[which(df_QC_GS$year>df_QC_GS$year[drought_end_2] &
                              df_QC_GS$year<=df_QC_GS$year[drought_end_2]+legacy_years_length_2-1)]<-1
      droughts$legacy_years_length[droughts$year==df_QC_GS$year[drought_end_1]]<-legacy_years_length_1
      droughts$legacy_years_length[droughts$year==df_QC_GS$year[drought_end_2]]<-legacy_years_length_2
      droughts$no_record[droughts$year==df_QC_GS$year[drought_end_1]]<-0
      droughts$no_record[droughts$year==df_QC_GS$year[drought_end_2]]<-no_record_2
    }
    # ggplot(df_QC_GS)+geom_line(aes(doy,legacy))+facet_wrap(~year)
    folder_path_droughts<-paste0(rootpath,'/2nd_study/droughts_11.0_',case_run,'/')
    if (!dir.exists(folder_path_droughts)) {
      dir.create(folder_path_droughts)
    }
    write.csv(droughts,file = paste0(folder_path_droughts,site,'_droughts_',sd,'.csv'),row.names = F)

    # #flag legacy and non-legacy periods
    df_QC_GS$group<-0 #non-legacy
    df_QC_GS$group[df_QC_GS$legacy==1]<-1 #legacy
    temp_normal_year<-df_QC_GS %>% filter(legacy==1)
    normal_year<-unique(df_QC_GS$year)[!(unique(df_QC_GS$year) %in% unique(temp_normal_year$year))]
    #
    #
    # ###### running the quantification algorithm ######
    diff_normal_GPP<-NA
    diff_legacy_GPP<-NA
    var_ex_GPP<-rep(NA,length(normal_year))
    for(k in 1:length(normal_year)){
      uncertainty_year<-normal_year[k]
      diff_GPP<-quantify_legacy_effects_non_QC_uncertainty(data=df_QC_GS,random_normal_year=uncertainty_year,
                                                    figures_folder = figures_folder,site=site,vars = c(vars_rf_GPP,'WAI'),EVI_flag = 0)

      var_ex_GPP[k]<-diff_GPP$var_explained
      if (k==1){
        diff_normal_GPP<-diff_GPP$normal
        diff_legacy_GPP<-diff_GPP$legacy
      }else{
        diff_normal_GPP<-rbind(diff_normal_GPP,diff_GPP$normal)
        diff_legacy_GPP<-rbind(diff_legacy_GPP,diff_GPP$legacy)
      }
    }
    var_ex_GPP<-data.frame(var_ex=var_ex_GPP,year=normal_year)

    #plot(diff_GPP$legacy)
    folder_path_annual<-paste0(rootpath,'/2nd_study/annual_legacy_effects_11.0_',case_run,'/')
    if (!dir.exists(folder_path_annual)) {
      dir.create(folder_path_annual)
    }
    figure_file<-paste0(folder_path_annual,site,'_legacy_effects_annual.tiff')
    tiff(figure_file,width = 1600,height = 1200,units = 'px', res=200)
    a<-ggplot(legacy_weekly,aes(x=year,y=residual_GPP))+
      geom_boxplot()+
      stat_pvalue_manual(stat.test_one, label = 'p.signif')+
      ylab('legacy effects in GPP (gC m-2 d-1)')
    print(a)
    dev.off()

    # ggplot(diff_legacy_GPP)+
    #   geom_line(aes(doy,GPP_Anom,color='obs'))+
    #   geom_line(aes(doy,GPP_Anom_rf,color='rf'))+
    #   facet_wrap(~year)
    folder_path_df<-paste0(rootpath,'/2nd_study/df_QC_GS_11.0_',case_run,'/')
    if (!dir.exists(folder_path_df)) {
      dir.create(folder_path_df)
    }
    write.csv(df_QC_GS,file = paste0(folder_path_df,site,'_df_QC_GS_',sd,'.csv'),row.names = F)

    folder_path_results<-paste0(rootpath,'/2nd_study/results_11.0_',case_run,'/')
    if (!dir.exists(folder_path_results)) {
      dir.create(folder_path_results)
    }
    write.csv(diff_normal_GPP,file = paste0(folder_path_results,site,'_diff_normal_GPP_ending_',sd,'.csv'),row.names = F)
    write.csv(diff_legacy_GPP,file = paste0(folder_path_results,site,'_diff_legacy_GPP_ending_',sd,'.csv'),row.names = F)

    
  }, error = function(e) {
    folder_path_error<-paste0(rootpath,'/2nd_study/error_11.0_',case_run,'/')
    if (!dir.exists(folder_path_error)) {
      dir.create(folder_path_error)
    }
    outputFile <-file(paste0(folder_path_error,site,'_error.txt'))
    writeLines(as.character(e), outputFile)
    close(outputFile)
  })
  
  
  
  print(p)
}
