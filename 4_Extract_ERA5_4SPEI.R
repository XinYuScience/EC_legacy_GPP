# Load necessary libraries
dyn.load('/opt/ohpc/pub/libs/hwloc/lib/libhwloc.so.15')
dyn.load('/opt/ohpc/pub/libs/gnu9/openmpi4/hdf5/1.10.8/lib/libhdf5_hl.so.100')
library(ncdf4)
library(dplyr)

# Read site data (all sites)
sites <- read.csv('/Net/Groups/BGI/people/xyu/legacy_EC/1_all_sites_data_record.csv')
n_sites <- nrow(sites)

# Define the path to the NetCDF files
file_path <- "/Net/Groups/data_BGC/era5_land/e1/0d10_daily"

# Define variable names and corresponding NetCDF variable name prefixes
variables <- list(
  ssr      = "ssr.daily.fc.era5_land.3600.1800.",   # Surface solar radiation downwards
  str      = "str.daily.fc.era5_land.3600.1800.",     # Surface thermal radiation
  ssrd     = "ssrd.daily.fc.era5_land.3600.1800.",    # Surface solar radiation downward (alternative)
  t2m      = "t2m.daily.an.era5_land.3600.1800.",     # 2m temperature
  t2m_max  = "t2m_max.daily.an.era5_land.3600.1800.",  # 2m max temperature
  t2m_min  = "t2m_min.daily.an.era5_land.3600.1800.",  # 2m min temperature
  sp       = "sp.daily.an.era5_land.3600.1800.",       # Surface pressure
  tp       = "tp.daily.an.era5_land.3600.1800."        # Total precipitation
)

# Initialize a list to store results for each site
results_list <- vector("list", n_sites)
for (j in 1:n_sites) {
  # Create an empty data frame for each site
  results_list[[j]] <- data.frame(Date = as.Date(character()),
                                  ssr = numeric(),
                                  str = numeric(),
                                  ssrd = numeric(),
                                  t2m = numeric(),
                                  t2m_max = numeric(),
                                  t2m_min = numeric(),
                                  sp = numeric(),
                                  tp = numeric(),
                                  stringsAsFactors = FALSE)
}

# Loop through each year
for (year in 1980:2023) {
  
  # Determine number of days in the year
  days_in_year <- ifelse((year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0), 366, 365)
  dates_year <- seq(as.Date(paste0(year, "-01-01")), by = "day", length.out = days_in_year)
  
  # For each variable, read the file only once, extract the full grid time series,
  # then extract the time series for every site.
  # Store the results in a list of matrices (rows = sites, columns = days)
  year_data_list <- list()
  
  for (var in names(variables)) {
    file_name <- paste0(variables[[var]], year, ".nc")
    file <- file.path(file_path, var, file_name)
    
    if (file.exists(file)) {
      # Open the NetCDF file
      nc_data <- nc_open(file)
      
      # Get latitude and longitude arrays from file
      lats <- ncvar_get(nc_data, "latitude")
      lons <- ncvar_get(nc_data, "longitude")
      
      # Read the full variable array (expecting dimensions: [lon, lat, time])
      var_values <- ncvar_get(nc_data, var)
      
      # Initialize a matrix to hold the time series for all sites (rows) for this variable
      var_matrix <- matrix(NA, nrow = n_sites, ncol = days_in_year)
      
      if (length(dim(var_values)) == 3) {
        # Determine the nearest grid cell indices for each site
        site_lat_idx <- sapply(1:n_sites, function(j) which.min(abs(lats - sites$latitude[j])))
        site_lon_idx <- sapply(1:n_sites, function(j) which.min(abs(lons - sites$longitude[j])))
        
        # Extract the time series for each site
        for (j in 1:n_sites) {
          series <- var_values[ site_lon_idx[j], site_lat_idx[j], ]
          
          # Convert units if needed (J/m² to W/m²)
          if (var %in% c("ssr", "str", "ssrd")) {
            series <- series / 86400
          }
          
          # Check length; if not matching, fill with NA and warn
          if (length(series) != days_in_year) {
            warning(paste("Mismatch in data length for", var, "in year", year, "for site", sites$site[j]))
            series <- rep(NA, days_in_year)
          }
          var_matrix[j, ] <- series
        }
      } else {
        warning(paste("Unexpected dimensions for variable", var, "in year", year))
      }
      
      nc_close(nc_data)
      year_data_list[[var]] <- var_matrix
    } else {
      warning(paste("File missing:", file))
      # Fill with NA for all sites if file is missing
      year_data_list[[var]] <- matrix(NA, nrow = n_sites, ncol = days_in_year)
    }
  }
  
  # For each site, combine the extracted variables for this year into a data frame and append to results_list
  for (j in 1:n_sites) {
    temp_df <- data.frame(Date = dates_year,
                          ssr = year_data_list[["ssr"]][j, ],
                          str = year_data_list[["str"]][j, ],
                          ssrd = year_data_list[["ssrd"]][j, ],
                          t2m = year_data_list[["t2m"]][j, ],
                          t2m_max = year_data_list[["t2m_max"]][j, ],
                          t2m_min = year_data_list[["t2m_min"]][j, ],
                          sp = year_data_list[["sp"]][j, ],
                          tp = year_data_list[["tp"]][j, ])
    temp_df$site <- sites$site[j]
    
    # Append the year's data to the site's accumulated results
    results_list[[j]] <- bind_rows(results_list[[j]], temp_df)
  }
  
  print(paste("Processed year:", year))
}

# Write separate CSV files for each site
for (j in 1:n_sites) {
  site_name <- sites$site[j]
  output_file <- paste0('/Net/Groups/BGI/people/xyu/SPEI_all_sites/all_vars_', site_name, '_cpus.csv')
  write.csv(results_list[[j]], output_file, row.names = FALSE)
  print(paste("Completed processing for site", site_name, "and saved data to:", output_file))
}
