library(bigleaf)
library(dplyr)
library(zoo)
library(fitdistrplus)

pwm_and_lmom <- function(x) {
  # Remove NA
  x <- sort(x[!is.na(x)])
  N <- length(x)
  if(N < 5) {
    # Not enough data to do a stable fit
    return(NULL)
  }
  # Empirical Fi and the x_(i)
  Fi <- (seq_len(N) - 0.35)/N
  
  # Probability-Weighted Moments (we only really need w0, w1, w2 for 3-param fit)
  #   w0 = mean(x)
  #   w1 = 1/N sum_{i=1..N} (1 - Fi)^1 * x_i
  #   w2 = 1/N sum_{i=1..N} (1 - Fi)^2 * x_i
  w0 <- mean(x)
  w1 <- mean( (1 - Fi) * x )
  w2 <- mean( (1 - Fi)^2 * x )
  
  # Then L-moments are commonly expressed as:
  #   l1 = w0
  #   l2 = w0 - 2*w1
  #   l3 = w0 - 6*w1 + 6*w2
  #   t3 = l3 / l2   (L-skewness), etc. 
  #   (We only need up to l3 for the shape calculations)
  l1 <- w0
  l2 <- w0 - 2*w1
  l3 <- w0 - 6*w1 + 6*w2
  
  # Return them
  list(w0=w0, w1=w1, w2=w2, l1=l1, l2=l2, l3=l3)
}

lmom_to_loglogistic3 <- function(lmom) {
  # Extract L-mom values
  l1 <- lmom$l1
  l2 <- lmom$l2
  l3 <- lmom$l3
  
  # The formulas from the paper (summarized version):
  #   We define PWMs: w0, w1, w2. Then
  #    b = [ (2*w1 - w0) ] / [ 6*w1 - w0 - 6*w2 ]
  #    a = ...
  #    g = ...
  # But let's code them in terms of w0, w1, w2 to avoid confusion.
  
  w0 <- lmom$w0
  w1 <- lmom$w1
  w2 <- lmom$w2
  
  denom <- (6*w1 - w0 - 6*w2)
  if(abs(denom) < 1e-12) {
    return(NULL) # degenerate
  }
  b <- (2*w1 - w0) / denom
  
  # If b <= 0, the distribution form is suspect
  if(b <= 0) {
    return(NULL)
  }
  
  # a = ( (w0 - 2*w1) * b ) / ( Gamma(1 + 1/b)*Gamma(1 - 1/b) )
  #   but we can compute those gamma terms. We'll define a small helper:
  gammafn <- base::gamma
  
  # Watch out if 1 - 1/b <= 0 => gamma(negative) => not valid
  if( (1 + 1/b) <= 0 || (1 - 1/b) <= 0 ) {
    return(NULL)
  }
  top   <- (w0 - 2*w1) * b
  bot   <- gammafn(1 + 1/b)* gammafn(1 - 1/b)
  a_val <- top / bot
  
  # The location param g:
  #   g = w0 - a * gamma(1 + 1/b)*gamma(1 - 1/b)
  g_val <- w0 - a_val * bot
  
  # Edge case: if a_val <= 0, not valid
  if(a_val <= 0) {
    return(NULL)
  }
  
  out <- list(a = a_val, b = b, g = g_val)
  return(out)
}

# (C)  Log-logistic CDF, given (a,b,g).
cdf_loglog3 <- function(x, a, b, g) {
  # If x <= g, F(x)=0. Otherwise:
  # F(x) = 1 / [1 + (a/(x-g))^b]
  out <- rep(0, length(x))
  positive_mask <- which(x > g)
  ratio <- (a / (x[positive_mask] - g))^b
  out[positive_mask] <- 1 / (1 + ratio)
  out
}

# (D)  Standard Normal transform (Abramowitz & Stegun)
#      Same approach used for SPI/SPEI in many references
stnorm_transform <- function(p) {
  # p is in [0,1]. We map to z ~ Normal(0,1).
  # For p=0 or 1 exactly, saturate the tails.
  c0 <- 2.515517; c1 <- 0.802853; c2 <- 0.010328
  d1 <- 1.432788; d2 <- 0.189269; d3 <- 0.001308
  
  z <- rep(NA_real_, length(p))
  # saturate extremes:
  p[p<=0] <- 1e-15
  p[p>=1] <- 1 - 1e-15
  
  lower_mask <- (p <= 0.5)
  upper_mask <- !lower_mask
  
  # For p <=0.5 => negative tail
  ptail <- ifelse(lower_mask, p, 1 - p)
  W <- sqrt(-2 * log(ptail))  # common factor
  
  numerator   <- (c0 + c1*W + c2*W^2)
  denominator <- (1 + d1*W + d2*W^2 + d3*W^3)
  z0 <- W - numerator/denominator
  
  z[lower_mask] <- -z0[lower_mask]  # negative tail
  z[upper_mask] <-  z0[upper_mask]  # positive tail
  
  z
}

calc_spei_day <- function(x_vector) {
  # x_vector is the 90-day sums for a single DOY across all years
  # Might have many NAs (early or incomplete), or too few data to fit well
  # browser()
  n_valid <- sum(!is.na(x_vector))
  if(n_valid < 10) {
    # Not enough data to fit distribution
    return(rep(NA, length(x_vector)))
  }
  
  # (1) Compute L-moments from the valid subset
  idx_valid <- which(!is.na(x_vector))
  x_valid   <- x_vector[idx_valid]
  lm <- pwm_and_lmom(x_valid)
  if(is.null(lm)) {
    # Could not compute
    return(rep(NA, length(x_vector)))
  }
  
  # (2) Convert L-moments -> (a,b,g)
  dist_par <- lmom_to_loglogistic3(lm)
  if(is.null(dist_par)) {
    return(rep(NA, length(x_vector)))
  }
  
  a <- dist_par$a
  b <- dist_par$b
  g <- dist_par$g
  
  # (3) For each valid x, compute CDF => standard normal => z
  F_valid <- cdf_loglog3(x_valid, a, b, g)
  z_valid <- stnorm_transform(F_valid)
  
  # (4) Put them back in the same positions
  z_out <- rep(NA, length(x_vector))
  z_out[idx_valid] <- z_valid
  z_out
}

sites <- read.csv('/Net/Groups/BGI/people/xyu/legacy_EC/1_all_sites_data_record.csv')

for (i in 1:nrow(sites)) {
  site<-sites$site[i]
  data<-read.csv(paste0('/Net/Groups/BGI/people/xyu/SPEI_all_sites/all_vars_',site,'_cpus.csv'))
  data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
  data$year <- format(data$Date, "%Y")
  data$doy <- as.numeric(format(data$Date, "%j"))
  data<-data %>% dplyr::filter(year<=2022 & year>=1982)
  data$sp<-data$sp/1000 # Pa --> kPa
  data$Rn<-data$ssr+data$str # net radiation
  data$Tair<-data$t2m-273.15 # K --> deg C
  
  #potential evaporation based on the Priestley-Taylor equation
  test<-potential.ET(data = data,Tair = 'Tair',pressure = 'sp',Rn='Rn',approach = "Priestley-Taylor")
  data$PET<-test$ET_pot*86400 # unit conversion kg m-2 s-1 --> mm d-1
  
  #climate water deficit calculation: precipitation - potential evaporation
  data$CWD_PT<-data$tp-data$PET
  
  # Now we apply it day-by-day.
  scale<-90
  CWD<-'CWD_PT'
  data <- data %>%
    mutate(!!sym(paste0('CWD_',scale)) := rollapply(get(CWD), 
                                                    width = scale, 
                                                    FUN = sum, 
                                                    align = "right", 
                                                    fill = NA, 
                                                    partial = TRUE)) %>%
    filter(row_number() >= scale) %>%
    group_by(doy) %>%
    mutate(!!sym(paste0('SPEI_',scale)) := calc_spei_day(!!sym(paste0('CWD_',scale)))) %>%
    ungroup()
  write.csv(data,paste0('/Net/Groups/BGI/people/xyu/SPEI_all_sites/SPEI_',site,'.csv'),row.names = FALSE)
}