library(sf)
library(tidyverse)
library(ggplot2)
library(ggspatial)
library(raster)
library(googledrive)
library(dplyr)
library(terra)
library(httr)


###### THIS FILE CREATES THE AERIAL IMAGE DATA SET FOR NY ##########


setwd('C:/Users/ayush/OneDrive/Desktop/Georgetown/NN/Data/NY')
store_path = "D:/blocks/NY"

bg_ny = read_sf("./block_group_shape/tl_2019_36_bg.shp")
bg_ny <- st_make_valid(bg_ny)
bg_crs <- st_crs(bg_ny)

ny_boundary = read_sf("./Boundaries - City/geo_export_5c7d0ec2-20e5-4170-9d8f-263e062e08b7.shp")
ny_boundary <- st_transform(ny_boundary, st_crs(bg_ny))

bg <- st_intersection(bg_ny, ny_boundary)

uhi_d1 <- stack("./Annual_gridded_UHI_data/Annual_gridded_UHI_data.daytime_UHI_2020.tif") %>% 
  projectRaster(crs = 32618) %>% 
  as.data.frame(xy = TRUE) 

uhi_n1 <- stack("./Annual_gridded_UHI_data/Annual_gridded_UHI_data.nighttime_UHI_2020.tif")%>% 
  projectRaster(crs = 32618) %>% 
  as.data.frame(xy = TRUE) 

uhi_d2 <- stack("./Annual_gridded_UHI_data (1)/Annual_gridded_UHI_data.daytime_UHI_2020.tif")%>% 
  projectRaster(crs = 32618) %>% 
  as.data.frame(xy = TRUE) 

uhi_n2 <- stack("./Annual_gridded_UHI_data (1)/Annual_gridded_UHI_data.nighttime_UHI_2020.tif")%>% 
  projectRaster(crs = 32618) %>% 
  as.data.frame(xy = TRUE) 

uhi_d3 <- stack("./Annual_gridded_UHI_data (2)/Annual_gridded_UHI_data.daytime_UHI_2020.tif")%>% 
  projectRaster(crs = 32618) %>% 
  as.data.frame(xy = TRUE) 

uhi_n3 <- stack("./Annual_gridded_UHI_data (2)./Annual_gridded_UHI_data.nighttime_UHI_2020.tif")%>% 
  projectRaster(crs = 32618) %>% 
  as.data.frame(xy = TRUE) 

uhi_day  = bind_rows(uhi_d1, uhi_d2,uhi_d3)
uhi_night  = bind_rows(uhi_n1, uhi_n2,uhi_n3)

uhi = full_join(uhi_night,uhi_day, by = c('x','y'))

uhi_full = uhi %>% 
  filter(Annual_gridded_UHI_data.daytime_UHI_2020 > -900) %>% 
  filter(Annual_gridded_UHI_data.nighttime_UHI_2020 > -900) %>%
  mutate(average  = (Annual_gridded_UHI_data.daytime_UHI_2020 + Annual_gridded_UHI_data.nighttime_UHI_2020)/2 )%>% 
  dplyr::select(c(x,y,average))

boundary_utm <- st_transform(bg_ny, crs = 32618)
city_boundary_utm <- st_transform(ny_boundary, crs = 32618)

res = 500

bbox <- st_bbox(boundary_utm)
x_min <- bbox$xmin - (bbox$xmin %% res)  # Align to the nearest lower multiple of 500m
y_min <- bbox$ymin - (bbox$ymin %% res)
x_max <- bbox$xmax + (res - bbox$xmax %% res)
y_max <- bbox$ymax + (res - bbox$ymax %% res)

# Create a grid with 500m x 500m cells
x_range <- seq(from = x_min, to = x_max, by = res)
y_range <- seq(from = y_min, to = y_max, by = res)

grid <- expand.grid(x = x_range, y = y_range) %>%
  st_as_sf(coords = c("x", "y"), crs = 32618) %>%
  st_make_grid(cellsize = c(res, res), what = "polygons")  # Ensure using polygons for grid

grid_intersects <- st_intersects(grid, city_boundary_utm, sparse = FALSE)


intersects_vector <- apply(grid_intersects, 1, any)

# Subset the grid using this vector
grid_complete_cells <- grid[intersects_vector]

# Ensure the grid is valid
grid_complete_cells <- st_make_valid(grid_complete_cells)

plot(st_geometry(city_boundary_utm), main = "Chicago Boundary with Complete Grid Overlay")
plot(st_geometry(grid_complete_cells), add = TRUE, lwd = 0.1)

regular_df <- as.data.frame(grid_complete_cells)
grid_complete_cells_sf <- st_as_sf(regular_df, sf_column_name = "geometry", crs = 32618)

grid_complete_cells_sf <- grid_complete_cells_sf %>%
  mutate(id = sprintf("00%d", row_number())) 


combined_df <- data.frame()
for (i in 1:nrow(grid_complete_cells_sf)) {
  
  start_time <- Sys.time()
  cell <- grid_complete_cells_sf[i,]
  geoid <- as.character(cell$id)
  
  cell_geom <- st_geometry(cell)
  bbox <- st_bbox(cell_geom)
  
  # Filter the UHI data frame for points within this bounding box
  uhi_filtered <- uhi_full %>%
    filter(x >= bbox$xmin, x <= bbox$xmax, y >= bbox$ymin, y <= bbox$ymax) %>%
    mutate(GEOID = geoid)
  
  combined_df <- rbind(combined_df, uhi_filtered)
  
  end_time <- Sys.time()  
  iteration_time <- end_time - start_time
  
  print(paste("Iteration", i, "took", iteration_time, "seconds"))
}

grouped =  combined_df %>% group_by(GEOID) %>% summarise(average  =  mean(average, na.rm =  TRUE))
grouped_full = combined_df %>% filter(!(is.na(average)))

grid_complete_cells_sf = grid_complete_cells_sf %>% rename(GEOID = id) 
cells_filtered  = left_join(grouped_full, grid_complete_cells_sf, by = 'GEOID') %>% st_as_sf()
write.csv(as.data.frame(cells_filtered) %>% dplyr::select(average,GEOID), 'NY_labels_aeria.csv')

grid_complete_cells_sf_transformed <- st_transform(grid_complete_cells_sf %>% filter(GEOID %in% cells_filtered$GEOID), crs =st_crs(bg))

bbox_data <- data.frame(
  id = character(),
  xmin = numeric(),
  xmax = numeric(),
  ymin = numeric(),
  ymax = numeric(),
  stringsAsFactors = FALSE
)
# Loop through each grid cell
for (i in 1:nrow(grid_complete_cells_sf_transformed)) {
  # Extract the current cell
  cell <- grid_complete_cells_sf_transformed[i, ]
  
  # Extract the ID (if available)
  geoid <- as.character(cell$GEOID)
  
  # Get the geometry of the cell
  cell_geom <- st_geometry(cell)
  
  # Calculate the bounding box
  bbox <- st_bbox(cell_geom)
  
  # Append the data to the dataframe
  bbox_data <- rbind(bbox_data, data.frame(
    id = geoid,
    xmin = bbox$xmin,
    xmax = bbox$xmax,
    ymin = bbox$ymin,
    ymax = bbox$ymax
  ))
}

###ADD API KEY HERE FOR GOOGLE STATIC MAPS
api_key = ''

inspect_failed <- list()
for (i in 1:nrow(bbox_data)) {
  # Calculate center latitude and longitude
  geoid = bbox_data$id[i]
  xmin = bbox_data$xmin[i]
  xmax = bbox_data$xmax[i]
  ymax = bbox_data$ymax[i]
  ymin = bbox_data$ymin[i]
  
  center_lat <- (ymin +ymax) / 2
  center_lon <- (xmin+ xmax) / 2
  
  # Create the URL for the Google Maps Static API
  url <- sprintf("https://maps.googleapis.com/maps/api/staticmap?center=%f,%f&size=600x300&maptype=satellite&visible=%f,%f|%f,%f|%f,%f|%f,%f&key=%s",
                 center_lat, center_lon, 
                 ymin, xmin, ymin, xmax, 
                 ymax, xmax, ymax, xmin, 
                 api_key)
  
  
  response <- GET(url)
  
  if (status_code(response) == 200) {
    
    file_path <- paste("D:/blocks/NY/aerial_images/", geoid, ".png", sep = "")
    print(file_path)
    writeBin(content(response, "raw"), file_path)
  } else {
    inspect_failed <- c(inspect_failed, geoid)
    print(sprintf("Failed to download image for GEOID: %s", geoid))
  }
}

# Display failed GEOIDs if any
if (length(inspect_failed) > 0) {
  print("Inspect download issues for the following GEOIDs:")
  print(inspect_failed)
}

xmin = -87.61842
xmax = -87.61237
ymin = 41.64007
ymax = 41.64461

center_lat <- (ymin + ymax) / 2
center_lon <- (xmin + xmax) / 2

api_key <- "AIzaSyDsI86z2nKnDH_cJszTbd9DTapvjAQGwZc"

# Create the URL
url <- sprintf("https://maps.googleapis.com/maps/api/staticmap?center=%f,%f&size=600x300&maptype=satellite&visible=%f,%f|%f,%f|%f,%f|%f,%f&key=%s",
               center_lat, center_lon, ymin, xmin, ymin, xmax, ymax, xmax, ymax, xmin, api_key)

response <- GET(url)

if (status_code(response) == 200) {
  # Write the content to an image file
  writeBin(content(response, "raw"), "C:/Users/ayush/OneDrive/Desktop/output_image.png")
} else {
  print("Failed to download image")
}











