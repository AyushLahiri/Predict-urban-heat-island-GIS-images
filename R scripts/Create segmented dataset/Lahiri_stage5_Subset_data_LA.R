library(sf)
library(tidyverse)
library(ggplot2)
library(ggspatial)
library(raster)
library(googledrive)
library(dplyr)

###### THIS FILE CREATES THE SEGMENTED DATA FOR LA ################

setwd('C:/Users/ayush/OneDrive/Desktop/Georgetown/NN/Data/LA')
store_path = "D:/blocks/LA"

bg_la = read_sf("./block_group_shape/USA_Block_Group.shp")
bg_la <- st_make_valid(bg_la)
bg_crs <- st_crs(bg_la)

la_boundary = read_sf('./Boundaries - City/County_Boundary.shp')

la_boundary = la_boundary %>% st_as_sf(crs = bg_crs)
la_boundary <- st_transform(la_boundary, st_crs(bg_la))
la_boundary <- st_make_valid(la_boundary)

bg <- st_intersection(bg_la, la_boundary)

#########################################################################
buildings = read_sf('./buildings/e63e5597-6c0d-464f-8ba1-a7288771575e2020330-1-h41qrd.fsqij.shp')
buildings <- st_transform(buildings, crs = bg_crs)
buildings <- st_make_valid(buildings)
###########################################################################


roads = read_sf("./roads/Streets.shp")
###########################################################################

aerial_image <- stack("./NLCD_tree_canopy/nlcd_tcc_conus_2021_v2021-4_GsD39diV9z89d7M7aUfw.tiff")
aerial_image = projectRaster(aerial_image, crs = st_crs(bg)$wkt)
aerial_image_bbox <- st_bbox(aerial_image)
aerial_image_bbox_sf <- st_as_sfc(aerial_image_bbox)

ndvi_df = as.data.frame(aerial_image, xy = TRUE) %>% filter(Layer_1 != 0)
min_value <- min(ndvi_df$Layer_1, na.rm = TRUE)
max_value <- max(ndvi_df$Layer_1, na.rm = TRUE)
mean <- mean(ndvi_df$Layer_1, na.rm = TRUE)
#########################################################################


uhi_day <- stack("./Annual_gridded_UHI_data.daytime_UHI_2020.tif")
uhi_night <- stack("./Annual_gridded_UHI_data.nighttime_UHI_2020.tif")
uhi_day = projectRaster(uhi_day, crs = st_crs(bg)$wkt)
uhi_night = projectRaster(uhi_night, crs = st_crs(bg)$wkt)

uhi_day_df = as.data.frame(uhi_day, xy = TRUE) 
uhi_night_df = as.data.frame(uhi_night, xy = TRUE) 

uhi = full_join(uhi_night_df,uhi_day_df, by = c('x','y'))

uhi_full = uhi %>% 
  filter(Annual_gridded_UHI_data.daytime_UHI_2020 > -900) %>% 
  filter(Annual_gridded_UHI_data.nighttime_UHI_2020 > -900) %>%
  mutate(average  = (Annual_gridded_UHI_data.daytime_UHI_2020 + Annual_gridded_UHI_data.nighttime_UHI_2020)/2 )%>% 
  dplyr::select(c(x,y,average))

combined_df <- data.frame()
for (i in 1:nrow(bg)) {
  
  block_group <- bg[i,]
  geoid <- as.character(block_group$FIPS)
  
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
write.csv(grouped_full, 'la_labels.csv')
############################################################################
bg_filtered  = left_join(grouped_full, bg, by = c('GEOID' = 'FIPS')) %>% st_as_sf()
# files_list <- list.files(path = 'D:/blocks/buildings')
# bg_filtered <- bg_filtered %>% filter(!GEOID %in% files_list)
############################################################################

############################################################################
for (i in 1:nrow(bg_filtered)) {
  # block_group  <- bg_filtered %>% filter(GEOID == 170311203004)
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
  
  # Create directories
  dir.create(buildings_dir, recursive = TRUE)
  dir.create(streets_dir, recursive = TRUE)
  dir.create(images_dir, recursive = TRUE)
  
  # Define file paths
  buildings_sub_name <- paste(buildings_dir, "/buildings_subset_", geoid, ".shp", sep = "")
  roads_sub_name <- paste(streets_dir, "/streets_subset_", geoid, ".shp", sep = "")
  plot_filename <- paste(images_dir, "/image_", geoid, ".png", sep = "")
  
  # Write the subsets to shapefiles, only if they exist
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

