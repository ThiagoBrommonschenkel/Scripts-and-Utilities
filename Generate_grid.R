##### This script was created by Gabriel Blasques and modified by Thiago Brommonschenkel.#####
generate_grid <- function(resolution, state = NULL, country = NULL, shapefile_path = NULL, save = FALSE, plot = FALSE) {
  start_time <- Sys.time() # Inicio da medição do tempo
  # Loading packages
  required_packages <- c("sf", "dplyr", "rnaturalearth", "ggplot2", "geobr")
  sapply(required_packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  })
  
  # Grid resolution 
  resolution <- as.numeric(resolution)
  
  # Specified geographical area 
  area <- NULL
  grid_name <- NULL
  if (!is.null(shapefile_path)) {
    area <- st_read(shapefile_path)
    grid_name <- paste0("Grid_", resolution, "_Shapefile")
  } else if (!is.null(state) && !is.null(country)) {
    state_data <- ne_states(country = country, returnclass = "sf")
    area <- state_data[state_data$name == state, ]
    if (nrow(area) == 0) stop("No such state found in the data")
    grid_name <- paste0("Grid_", resolution, "_", state, "_", country)
  } else if (!is.null(country)) {
    country_data <- ne_countries(scale = "medium", returnclass = "sf")
    area <- country_data[country_data$admin == country, ]
    if (nrow(area) == 0) stop("No such country found in the data")
    grid_name <- paste0("Grid_", resolution, "_", country)
  } else {
    stop("You must specify either a country, both a state and a country, or a shapefile path")
  }
  
  # Ensure the area is in the same CRS as our grid
  area <- st_transform(area, crs = 4326)
  
  # Bounding box 
  bbox <- st_bbox(area)
  lon_seq <- seq(from = bbox["xmin"], to = bbox["xmax"], by = resolution)
  lat_seq <- seq(from = bbox["ymin"], to = bbox["ymax"], by = resolution)
  
  # Grid 
  grid_points <- expand.grid(lon = lon_seq, lat = lat_seq)
  grid_points_sf <- st_as_sf(grid_points, coords = c("lon", "lat"), crs = 4326)
  grid_points_sp <- st_intersection(grid_points_sf, st_transform(area, crs = 4326))
  
  grid_points_df <- as.data.frame(st_coordinates(grid_points_sp))
  names(grid_points_df) <- c("LON", "LAT")
  
  grid_points_df$LON <- format(as.numeric(grid_points_df$LON), digits = 8, nsmall = 8)
  grid_points_df$LAT <- format(as.numeric(grid_points_df$LAT), digits = 8, nsmall = 8)
  
  # Collect names of municipalities and states
  municipios <- read_municipality(code_muni = "all", year = 2020)
  estados <- read_state(year = 2020)
  
  # Ensure municipios and estados are in the same CRS as our grid
  municipios <- st_transform(municipios, crs = 4326)
  estados <- st_transform(estados, crs = 4326)
  
  grid_points_sf <- st_as_sf(grid_points_df, coords = c("LON", "LAT"), crs = 4326, remove = FALSE)
  
  # Identify municipality and state for each point
  municipios_inter <- st_join(grid_points_sf, municipios, join = st_intersects)
  estados_inter <- st_join(grid_points_sf, estados, join = st_intersects)
  
  grid_points_df$MUNICIPIO <- municipios_inter$name_muni
  grid_points_df$ESTADO <- estados_inter$abbrev_state
  
  # Save
  if (save) {
    write.csv(grid_points_df, file = paste0(grid_name, ".csv"), row.names = FALSE)
  }
  
  # Plot
  if (plot) {
    p <- ggplot() +
      geom_sf(data = area, fill = NA, color = "black") +
      geom_point(data = grid_points_df, aes(x = as.numeric(LON), y = as.numeric(LAT)), color = "blue", size = 0.5) +
      coord_sf() +
      theme_minimal() +
      labs(title = "Grid Points", x = "Longitude", y = "Latitude")
    print(p)
  }
  
  assign(grid_name, grid_points_df, envir = .GlobalEnv)
  
  end_time <- Sys.time()  
  time_taken <- end_time - start_time 
  
  cat("Tempo gasto pelo script:", time_taken, "\n")
  
  invisible(list(grid_name = grid_name, grid_points_df = grid_points_df))
}