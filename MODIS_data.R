########## This script was created by Thiago Brommonschenkel, modifying and ######### 
######### applying functions from the MODISTools package (Koen Hufkens, 2023)######### 
######### to download data from NASA's MODIS platform.######### 
######### for questions and suggestions: Thiago.brommonschenkel@gmail.com #########  

# Downloading packages if necessary
if (!require("MODISTools")) install.packages("MODISTools")
if (!require("dplyr")) install.packages("dplyr")

# Loading the packages
library(MODISTools)
library(dplyr)

# Function to check data availability
check_data_availability <- function(product, lat, lon, start, end) {
  dates <- tryCatch(
    MODISTools::mt_dates(product = product, lat = lat, lon = lon),
    error = function(e) NULL
  )
  if (is.null(dates)) return(FALSE)
  available_dates <- as.Date(dates$calendar_date)
  return(any(available_dates >= start & available_dates <= end))
}

# Function to download MODIS data for each environment
download_modis <- function(df, product, band) {
  results <- apply(df, 1, function(x) {
    if (check_data_availability(product, as.numeric(x["lat"]), as.numeric(x["lon"]), as.Date(x["plant"]), as.Date(x["harv"]))) {
      result <- MODISTools::mt_subset(
        site_name = as.character(x["env"]), 
        product = product, 
        band = band, 
        lat = as.numeric(x["lat"]), 
        lon = as.numeric(x["lon"]), 
        start = as.character(x["plant"]), 
        end = as.character(x["harv"]), 
        km_lr = 0, 
        km_ab = 0, 
        internal = TRUE, 
        progress = FALSE
      )
      return(result)
    } else {
      message(sprintf("No data available for env %s in the selected date range.", x["env"]))
      return(NULL)
    }
  })
  return(do.call(rbind, results))
}

#To see the bands and products available
bands = mt_bands()
products = mt_products()

# Applying the function
data_modis <- download_modis(df = coords, product = 'MYD13Q1', band = "250m_16_days_NDVI")