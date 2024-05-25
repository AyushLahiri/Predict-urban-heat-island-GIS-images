library(sf)
library(tidyverse)
library(ggplot2)
library(ggspatial)
library(raster)
library(googledrive)
library(dplyr)

###### THIS FILE CREATES THE SEGMENTED DATA FOR NY ################


setwd('C:/Users/ayush/OneDrive/Desktop/Georgetown/NN/Data/NY')
store_path = "D:/blocks/NY"

bg_ny = read_sf("./block_group_shape/tl_2019_36_bg.shp")
bg_ny <- st_make_valid(bg_ny)
bg_crs <- st_crs(bg_ny)

ny_boundary = read_sf("./Boundaries - City/geo_export_5c7d0ec2-20e5-4170-9d8f-263e062e08b7.shp")
ny_boundary <- st_transform(ny_boundary, st_crs(bg_ny))

bg <- st_intersection(bg_ny, ny_boundary)

#########################################################################
buildings = read_sf("./buildings/building.shp")
buildings <- st_transform(buildings, crs = bg_crs)
buildings <- st_make_valid(buildings)
###########################################################################
roads = read_sf("./roads/geo_export_119b7bc3-cab0-4421-8498-81587df350de.shp")
roads <- st_transform(roads, st_crs(bg_ny))
###########################################################################

aerial_image <- stack("./NLCD_tree_canopy/nlcd_tcc_conus_2021_v2021-4_FtIcdHp7tBJwy9NrRgET.tiff")
aerial_image = projectRaster(aerial_image, crs = st_crs(bg)$wkt)
aerial_image_bbox <- st_bbox(aerial_image)
aerial_image_bbox_sf <- st_as_sfc(aerial_image_bbox)

ndvi_df = as.data.frame(aerial_image, xy = TRUE) %>% filter(Layer_1 != 0)
min_value <- min(ndvi_df$Layer_1, na.rm = TRUE)
max_value <- max(ndvi_df$Layer_1, na.rm = TRUE)
#########################################################################


uhi_d1 <- stack("./Annual_gridded_UHI_data/Annual_gridded_UHI_data.daytime_UHI_2020.tif") %>% 
  projectRaster(crs = st_crs(bg)$wkt) %>% 
  as.data.frame(xy = TRUE) 

uhi_n1 <- stack("./Annual_gridded_UHI_data/Annual_gridded_UHI_data.nighttime_UHI_2020.tif")%>% 
  projectRaster(crs = st_crs(bg)$wkt) %>% 
  as.data.frame(xy = TRUE) 

uhi_d2 <- stack("./Annual_gridded_UHI_data (1)/Annual_gridded_UHI_data.daytime_UHI_2020.tif")%>% 
  projectRaster(crs = st_crs(bg)$wkt) %>% 
  as.data.frame(xy = TRUE) 

uhi_n2 <- stack("./Annual_gridded_UHI_data (1)/Annual_gridded_UHI_data.nighttime_UHI_2020.tif")%>% 
  projectRaster(crs = st_crs(bg)$wkt) %>% 
  as.data.frame(xy = TRUE) 

uhi_d3 <- stack("./Annual_gridded_UHI_data (2)/Annual_gridded_UHI_data.daytime_UHI_2020.tif")%>% 
  projectRaster(crs = st_crs(bg)$wkt) %>% 
  as.data.frame(xy = TRUE) 

uhi_n3 <- stack("./Annual_gridded_UHI_data (2)./Annual_gridded_UHI_data.nighttime_UHI_2020.tif")%>% 
  projectRaster(crs = st_crs(bg)$wkt) %>% 
  as.data.frame(xy = TRUE) 

uhi_day  = bind_rows(uhi_d1, uhi_d2,uhi_d3)
uhi_night  = bind_rows(uhi_n1, uhi_n2,uhi_n3)

uhi = full_join(uhi_night,uhi_day, by = c('x','y'))

uhi_full = uhi %>% 
  filter(Annual_gridded_UHI_data.daytime_UHI_2020 > -900) %>% 
  filter(Annual_gridded_UHI_data.nighttime_UHI_2020 > -900) %>%
  mutate(average  = (Annual_gridded_UHI_data.daytime_UHI_2020 + Annual_gridded_UHI_data.nighttime_UHI_2020)/2 )%>% 
  dplyr::select(c(x,y,average))

combined_df <- data.frame()
for (i in 1:nrow(bg)) {
  
  block_group <- bg[i,]
  geoid <- as.character(block_group$GEOID)
  
  block_group_geom <- st_geometry(block_group)
  bbox <- st_bbox(block_group_geom)
  
  # Filter the UHI data frame for points within this bounding box
  uhi_filtered <- uhi_full %>%
    filter(x >= bbox$xmin, x <= bbox$xmax, y >= bbox$ymin, y <= bbox$ymax) %>%
    mutate(GEOID = geoid)
  
  combined_df <- rbind(combined_df, uhi_filtered)
  
}

grouped =  combined_df %>% group_by(GEOID) %>% summarise(average  =  mean(average, na.rm =  TRUE))
grouped_full = grouped %>% filter(!(is.na(average)))
write.csv(grouped_full, 'ny_labels.csv')

############################################################################
bg_filtered  = left_join(grouped_full, bg, by = 'GEOID') %>% st_as_sf()
files_list <- list.files(path = 'D:/blocks/buildings')
bg_filtered <- bg_filtered %>% filter(!GEOID %in% files_list)
############################################################################

############################################################################
for (i in 1:nrow(bg_filtered)) {
  block_group <- bg_filtered[i,]
  block_group_sp <- as(st_as_sf(block_group), 'Spatial')
  
  
  
  block_group_geom <- st_geometry(block_group)
  block_group_bbox_sf <- st_as_sfc(st_bbox(block_group_geom))
  
  overlap <- st_intersects(aerial_image_bbox_sf, block_group_bbox_sf, sparse = FALSE)[1,1]
  
  if (overlap == TRUE){
    masked_image <- mask(aerial_image, block_group_sp)
  }
  
  buildings_subset <- buildings[st_intersects(buildings, block_group, sparse = FALSE), ]
  
  print(nrow(buildings_subset))
  print(overlap)
  
  if (nrow(buildings_subset) == 0 && overlap ==  FALSE) {
    next
  }
  
  roads_clipped <- st_intersection(roads, block_group)
  roads_clipped <- roads_clipped[st_geometry_type(roads_clipped) %in% c("LINESTRING", "MULTILINESTRING"), ]
  roads_subset <- st_transform(roads_clipped, crs = st_crs(bg))
  
  geoid <- as.character(block_group$GEOID)
  buildings_dir <- paste(store_path,"buildings", geoid, sep = "/")
  streets_dir <- paste(store_path,"streets", geoid, sep = "/")
  images_dir <- paste(store_path,"images", sep = "/")
  

  dir.create(buildings_dir, recursive = TRUE)
  dir.create(streets_dir, recursive = TRUE)
  dir.create(images_dir, recursive = TRUE)
  
  buildings_sub_name <- paste(buildings_dir, "/buildings_subset_", geoid, ".shp", sep = "")
  roads_sub_name <- paste(streets_dir, "/streets_subset_", geoid, ".shp", sep = "")
  plot_filename <- paste(images_dir, "/image_", geoid, ".png", sep = "")
  
  st_write(buildings_subset, buildings_sub_name, delete_layer = TRUE)
  st_write(roads_subset, roads_sub_name, delete_layer = TRUE)
  
  
  
  # Prepare data frame for green cover if exists
  if (overlap) {
    canopy_df <- as.data.frame(masked_image, xy = TRUE) %>% filter(Layer_1 != 0)
  }
  
  # Create the plot
  plot <- ggplot() +
    scale_fill_gradientn(colors = c("lightgreen", "darkgreen"),
                         values = scales::rescale(c(min_value, max_value)),
                         limits = c(min_value, max_value)) +
    theme_void() +
    theme(legend.position = "none")
  
  if (overlap){
    plot <- plot + geom_raster(data = canopy_df, aes(x = x, y = y, fill = Layer_1))
  }

  plot <- plot + geom_sf(data = roads_subset, color = 'blue', alpha = 0.8)
  plot <- plot + geom_sf(data = buildings_subset, fill = "coral", alpha = 0.5)
  plot <- plot + geom_sf(data = block_group, fill = "grey", alpha = 0.2)
  

  ggsave(filename = plot_filename, plot = plot, width = 5, height = 5, dpi = 200)
  
  print ('done')
  print('---------------------------------------------')
  
  if (i%%100 == 0){
    print('------------------------------------------')
    print(paste(i,"number of rows done"))
    print('------------------------------------------')
  }
}
