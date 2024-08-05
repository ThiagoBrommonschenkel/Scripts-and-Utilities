########## This script was created by Thiago Brommonschenkel, modifying and ######### 
######### applying functions from the MODISTools package (Koen Hufkens, 2023)######### 
######### to download data from NASA's MODIS platform.######### 
######### For questions and suggestions: Thiago.brommonschenkel@gmail.com #########  

# Downloading packages if necessary
if (!require("MODISTools")) install.packages("MODISTools")
if (!require("dplyr")) install.packages("dplyr")
if (!require("parallel")) install.packages("parallel")

# Loading the packages
library(MODISTools)
library(dplyr)
library(parallel)

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
download_modis <- function(df, product, bands) {
  df <- df %>% filter(!is.na(lat) & !is.na(lon) & !is.na(plant) & !is.na(harv))
  
  results <- mclapply(1:nrow(df), function(i) {
    x <- df[i, ]
    lat <- as.numeric(x["lat"])
    lon <- as.numeric(x["lon"])
    start <- as.Date(as.character(x["plant"]))
    end <- as.Date(as.character(x["harv"]))
    
    if (is.na(lat) || is.na(lon) || is.na(start) || is.na(end)) {
      message(sprintf("Missing data for env %s.", x["env"]))
      return(NULL)
    }
    
    if (check_data_availability(product, lat, lon, start, end)) {
      band_results <- lapply(bands, function(band) {
        MODISTools::mt_subset(
          site_name = as.character(x["env"]), 
          product = product, 
          band = band, 
          lat = lat, 
          lon = lon, 
          start = as.character(start), 
          end = as.character(end), 
          km_lr = 0, 
          km_ab = 0, 
          internal = TRUE, 
          progress = FALSE
        )
      })
      return(do.call(rbind, band_results))
    } else {
      message(sprintf("No data available for env %s in the selected date range.", x["env"]))
      return(NULL)
    }
  }, mc.cores = detectCores() - 1)
  
  results <- do.call(rbind, results)
  return(results)
}

# Verificar produtos disponíveis
products <- MODISTools::mt_products()
print(products)

# Selecionar produto (por exemplo, 'MYD13Q1')
selected_product <- 'MYD13Q1'

# Verificar bandas disponíveis para o produto selecionado
bands <- MODISTools::mt_bands(product = selected_product)
print(bands)

# Exemplo de dados coords
coords <- data.frame(
  env = c("Env1", "Env2"),
  lat = c(-15.7801, -23.5505),
  lon = c(-47.9292, -46.6333)
)

# Aplicando a função
start = "2000-02-01"
end = "2024-08-01"
coords$plant = start
coords$harv = end
coords$lat = as.numeric(coords$lat)
coords$lon = as.numeric(coords$lon)

# Lista de bandas disponíveis
band_list <- bands$band

# Baixar dados MODIS para todas as bandas disponíveis
data_modis <- download_modis(df = coords, product = selected_product, bands = band_list)

# Check the result
print(data_modis)