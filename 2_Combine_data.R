## Plot all the first detections in ArcGIS Pro

library(dplyr)
library(fs)
library(stringr)
library(ggplot2)
library(viridis)
library(sf)
library(rnaturalearth)
library(maps)
library(FedData)
library(terra)
library(tidyr)
library(gridExtra)

path = "./Screened Data/"
merged_df = data.frame()

# combine all screening files and filter based on notes

for (i in 1:length(dir_ls(path))){
  file = read.csv(dir_ls(path)[i]) %>% 
    filter(!(day_notes %in% c("aw", "ap", "weather", "dr/ap"))) %>% 
    filter(label == "swallow-roost" | label == "bad-track") %>%
    select(-one_of("geo_dist")) # some files don't have this column
  merged_df = rbind(merged_df, file)
}

merged_df %>% distinct(day_notes) %>% print()
merged_df %>% filter(label=="bad-track") %>% distinct(notes) %>% print()

##############################################################################
# cut the bad-track (cut to the number or to half (if NA))
##############################################################################

merged_df$notes = as.numeric(gsub("[^0-9]", "", merged_df$notes))# Extract only the numbers
merged_df %>% distinct(notes) %>% print()
length(unique(merged_df$track_id))

detections = data.frame()
for (i in unique(merged_df$track_id)){
  
  df = merged_df %>% filter(track_id == i)
  
  if (df$label[1]=="bad-track" & is.na(df$notes[1])) {
    filtered_df <- df %>%
      filter(row_number(df$from_sunset) < nrow(df)/2+1)
  } else if (df$label[1]=="bad-track" && is.numeric(df$notes[1])) {
    filtered_df <- df %>%
      filter(row_number(df$from_sunset) <= df$notes[1])
  } else {filtered_df = df}
  
  detections = rbind(detections, filtered_df)
}

write.csv(detections, "./Data/1_all_detections.csv", row.names=FALSE)
length(unique(detections$track_id))
# Get the first detections for plotting and mean shift clustering algorithm

first_df = detections %>% 
  group_by(track_id) %>% 
  filter(row_number(from_sunset) == 1)
length(unique(first_df$track_id))
##############################################################################
# convert points with lat and lon from EPSG:4326 to a equal distance projection
##############################################################################

# Load required library
library(sf)

# Sample data frame with latitude and longitude
data <- first_df[8:9]

# Create an sf object from the data frame, specifying EPSG:4326
data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)

# Define target projection (e.g., equal-distance projection, like EPSG:6933)
# Then, transform the sf object to the new projection
equal_distance_projection <- st_transform(data_sf, crs = 6933)

# View the transformed data
print(equal_distance_projection)

# Add the projected coordinates back to your data frame
first_df$aea_lon <- st_coordinates(equal_distance_projection)[,1]
first_df$aea_lat <- st_coordinates(equal_distance_projection)[,2]

write.csv(first_df, "./Data/2_firstdetections.csv", row.names=FALSE)


##############################################################################
# Mean Shift Clustering (not working, use python instead)
##############################################################################

# library(meanShiftR)
# library(ggplot2)
# library(viridis)
# library(cluster)
# ggplot(first_df,aes(lon,lat,color=station))+
#   geom_point()+
#   scale_color_viridis(discrete = TRUE, alpha = 0.2) +
#   theme_bw()
# 
# x <- data.matrix(first_df[8:9])
# 
# ## form a set of candidate bandwidths
# bandwidths <- quantile( dist(x), seq( 0.05, 0.40, by=0.05 ) )
# 
# classification <- meanShift(x, 
#                             algorithm="KDTREE", 
#                             iterations = 1,
#                             bandwidth = rep(29264, 2))
# 
# unique(classification$assignment)
# 
# #visualize the clustering
# classification_df = data.frame(x, roost = classification$assignment)
# 
# ggplot(classification_df, aes(lon,lat,color=roost))+
#   geom_point()+
#   scale_color_viridis(discrete = FALSE, alpha = 0.2) +
#   theme_bw()
# 
# # Compute or Extract Silhouette Information from Clustering
# # Silhouette coefficient measures how similar an object i is to the other objects in its own cluster versus those in its neighbour cluster. Values close to 1 indicate the object is well clustered
# str(si <- silhouette(classification$assignment, dist(x, "canberra")))
# (ssi <- summary(si))
# plot(si) # silhouette plot

length(unique(cluster_df_NOduplicate$track_id))
length(unique(X3_1_firstdetections_clustered$track_id))
X4_firstdetections_unduplicated[is.na(X4_firstdetections_unduplicated$cluster_ID), ]
X3_1_firstdetections_clustered[is.na(X3_1_firstdetections_clustered$cluster_ID), ]

##############################################################################
# Delete duplicate detection
##############################################################################

cluster_df = read.csv("./Data/3_firstdetections_clustered.csv")

#calculate the distance from radar
radar_texas = read.csv("./Data/radar_texas.csv")[,c("SITE", "LATITUDE_N", "LONGITUDE_W")]
names(radar_texas) = c("station", "lat_radar", "lon_radar")
cluster_df = merge(cluster_df, radar_texas, by="station")
library(geosphere)

cluster_df = cluster_df %>% mutate(filename_det_idx = paste0(filename, "_", str_sub(track_id, start= 14)))
cluster_df = left_join(cluster_df, track.df[c("filename_det_idx", "geo_dist")], by = "filename_det_idx", multiple = "first")

cluster_df = cluster_df %>%
  mutate(dist = distHaversine(cbind(lon, lat), cbind(lon_radar, lat_radar)))

cluster_df$dist_diff = cluster_df$geo_dist - cluster_df$dist
hist(cluster_df$dist_diff)

cluster_df_NOduplicate = data.frame()

for (i in unique(cluster_df$cluster_ID)){
  df = cluster_df %>% filter(cluster_ID == i)
  if (length(unique(df$station))>1){
    
    if (i==20){
      df = df %>% filter(station=="KSJT") #Eckert James River Cave needs to use the data from KSJT
    }
    else{
      table = df %>% group_by(station) %>% summarize (mean(dist))
      kept_station = as.character(table[which.min(table$`mean(dist)`),1]) # keep the detections closer to the radar
      df = df %>% filter(station==kept_station)
    }
  } else {
    df = df
  }
  cluster_df_NOduplicate = rbind(cluster_df_NOduplicate, df)
}

write.csv(cluster_df_NOduplicate, "./Data/4_firstdetections_unduplicated.csv", row.names=FALSE)

##############################################################################
# Create different sizes of buffers around each points groups
# Calculate proportional landcover
##############################################################################

us = vect('./Data/US_State_Boundaries/US_State_Boundaries.shp')
texas = us[us$NAME == "Texas"]

texas = vect("./Data/SC_Texas/SC_Texas.shp")
plot(texas)

NLCD <-
  get_nlcd(
    template = texas,
    label = "texas",
    year = 2021
  )

# Plot with terra::plot
plot(NLCD)
writeRaster(NLCD, "./Data/Texas_NLCD.tif", overwrite=TRUE)

NLCD = rast("./Data/Texas_NLCD.tif")
ggplot(cluster_df_NOduplicate, aes(aea_lon, aea_lat, color=cluster_ID))+
  geom_point()+
  scale_color_viridis(alpha = 0.2) +
  theme_bw()

landcover_df = data.frame()
for (i in unique(cluster_df_NOduplicate$cluster_ID)){
  data_sf <- st_as_sf(cluster_df_NOduplicate %>% filter(cluster_ID == i), coords = c("lon", "lat"), crs = 4326)
  equal_data_sf <- st_transform(data_sf, crs = 5070)
  # create a 1km around each point then dissolve them
  buf <- st_buffer(equal_data_sf, dist = 1000) %>% st_union()
  buf = vect(buf)
  plot(buf)
  # calculate the percent area for each land cover type within the dissolved buffer
  extract.df = terra::extract(NLCD, buf)
  
  prop.lc = extract.df %>%
    setNames(c("ID", "lc_type")) %>%        # rename for ease
    group_by(ID, lc_type) %>%               # group by point (ID) and lc class 
    summarise(n = n()) %>%                  # count the number of occurences of each class
    mutate(pland = n / sum(n)) %>%          # calculate percentage
    ungroup() %>%                           # convert back to original form
    dplyr::select(ID, lc_type, pland) %>%   # keep only these vars
    complete(ID, nesting(lc_type), 
             fill = list(pland = 0)) %>%             # fill in implicit landcover 0s
    spread(lc_type, pland)                 # convert to long format
  prop.lc$ID = i
  
  landcover_df = dplyr::bind_rows(landcover_df, prop.lc)
}


write.csv(landcover_df, "./Data/5_landcover_df.csv", row.names=FALSE)

#combine several nlcd categories
landcover_df$Developed = rowSums(landcover_df[ , c(3,4,5,6)], na.rm=TRUE)
landcover_df$Barren = landcover_df[,7]
landcover_df$Forest = rowSums(landcover_df[ , c(8,9,10)], na.rm=TRUE)
landcover_df$Water = rowSums(landcover_df[ , c(2,15,16)], na.rm=TRUE)
landcover_df$Shrub_Harbaceous = rowSums(landcover_df[ , c(11,12)], na.rm=TRUE)
landcover_df$Cultivated = rowSums(landcover_df[ , c(13,14)], na.rm=TRUE)
landcover_agg = landcover_df[, c(18:23)]

landcover_agg$class = colnames(landcover_agg)[apply(landcover_agg,1,which.max)]
landcover_agg$ID = landcover_df$ID

write.csv(landcover_agg, "./Data/6_landcover_agg.csv", row.names=FALSE)

#manually correct landcover type for some cluster_id in Excel

landcover_agg = read.csv("./Data/6_landcover_agg.csv")

first_df_nlcd = merge(cluster_df_NOduplicate, landcover_agg, by.x="cluster_ID", by.y="ID")

first_df_nlcd[is.na(first_df_nlcd$cluster_ID), ]

write.csv(first_df_nlcd, "./Data/7_firstdetections_nlcd.csv", row.names=FALSE)


##############################################################################
# Merge counts into the csv
##############################################################################

# Outputs
# 600x600 jpg images
# (A) scans_SSSS_yyyymmdd_yyyymmdd.txt
# columns
# filename,local_time
# one scan per row
# grouped by local dates
# within each local date, ordered temporally
# 
# (B) tracks_SSSS_yyyymmdd_yyyymmdd.txt
# columns
# track_id,filename,from_sunrise,
# det_score,x,y,r,lon,lat,radius,geo_dist, → tracing boxes
# local_time
# one bounding box per row
# grouped by tracks, which are grouped by local dates
# 
# (C) sweeps_SSSS_yyyymmdd_yyyymmdd.txt
# columns
# track_id,filename,sweep_idx,sweep_angle,
# count_scaling,n_roost_pixels,n_overthresh_pixels,n_animals
# one sweep per row, count animals, more rows than (B)
# all angles less than 5km


sweep_path = "./Data/counts_40dBZ/sweeps/"
track_path = "./Data/counts_40dBZ/tracks/"
count_df = data.frame()

# combine all screening files and filter based on notes

# combine all sweep files
for (i in 1:length(dir_ls(sweep_path))){
  file = read.csv(dir_ls(sweep_path)[i]) 
  if (nrow(file) == 0){next}
  else{
    table = file %>% 
      mutate(filename_det_idx = paste0(filename, "_", str_sub(track_id, start= 14))) %>% 
      group_by(filename_det_idx) %>% 
      summarize(box_sum_bats = sum(n_animals))
    count_df = rbind(count_df, table)}
}

write.csv(count_df, "./Data/7.5_alltracks_counts_40dBZ.csv", row.names = F)

screened_df = read.csv("./Data/1_all_detections.csv") %>% 
  mutate(track = str_sub(track_id, start= 14)) %>%
  mutate(filename_det_idx = paste0(filename, "_", track))

first_df_nlcd = read.csv("./Data/7_firstdetections_nlcd.csv")

screened_df_count = left_join(screened_df, count_df, by="filename_det_idx")

# we have to delete the tracks that in not in the first_df_nlcd (since we deleted the duplicated files)
vars_only_in_df1 <- setdiff(unique(screened_df_count$track_id), unique(first_df_nlcd$track_id))
screened_df_count_filtered = screened_df_count[screened_df_count$track_id %in% unique(first_df_nlcd$track_id), ]

screened_df_count = left_join(screened_df_count_filtered, 
                              first_df_nlcd[c("cluster_ID", "track_id", "ctr_lon", "ctr_lat", "dist", "class")], 
                              by="track_id")
screened_df_count[is.na(screened_df_count$cluster_ID), ]


# Invesgating NAs
count_df[is.na(count_df$box_sum_bats), ]
first_df_nlcd[is.na(first_df_nlcd$class), ]
first_df_nlcd[is.na(first_df_nlcd$cluster_ID), ]
screened_df_count[is.na(screened_df_count$box_sum_bats), ] # why is there NAs for counts?
screened_df_count = screened_df_count[!is.na(screened_df_count$cluster_ID), ] #because we deleted duplicates
screened_df_count[is.na(screened_df_count$cluster_ID), ]
screened_df_count[is.na(screened_df_count$class), ]

write.csv(screened_df_count, "./Data/8_all_detections_w_counts.csv", row.names = F)



##############################################################################
# Examining high counts (need to figure out a way to eliminate weather contaminated scans, still a lot)
##############################################################################
#devtools::install_github("adokter/bioRad")
library(bioRad)
library(vol2birdR)
#install_mistnet()
#vol2birdR::install_mistnet_model()

df = read.csv("./Data/8_all_detections_w_counts.csv") %>% filter(box_sum_bats<7000000)
df$date = anytime::anydate(df$date) 
df$time = str_sub(df$filename, start = 14, end = 19)


ggplot(df %>% filter(box_sum_bats<6000000), aes(x=from_sunset, y = box_sum_bats))+
  geom_point()


ggplot(df %>% filter(track_id == "KGRK20220513-1"), aes(x=from_sunset, y = box_sum_bats))+
  geom_point()+
  geom_smooth()

###################################
#plot the high 100 counts in BioRad
###################################

df_high100 = top_n(df,100,box_sum_bats) %>% filter(date > '2012-01-01')
df_high100_add = df %>% filter(date > '2012-01-01') %>% top_n(100, box_sum_bats)
df_high100 = df_high100_add %>% top_n(-51, box_sum_bats)
write.csv(df_high100, "./Data/df_high100.csv", row.names = F)

image2xy = function(x, y, r, dim=600, rmax=150000){
  
  x0 = y0 = dim/2.0 # origin
  x =  (x - x0)*2*rmax/dim
  y = -(y - y0)*2*rmax/dim
  r = r*2*rmax/dim*1.2
  return(list(x = x, y = y, r = r))
}


for (i in 1:nrow(df_high100)){
  
  # DRAW a box around the roost
  l = image2xy(df_high100[i,]$x, df_high100[i,]$y, df_high100[i,]$r)
  x = l$x
  y = l$y
  r = l$r
  
  # Calculate the coordinates of the four corners
  top_left <- c(x - r, y + r)
  top_right <- c(x + r, y + r)
  bottom_right <- c(x + r, y - r)
  bottom_left <- c(x - r, y - r)
  
  # Create a data frame with the coordinates
  square_df <- data.frame(
    x = c(top_left[1], top_right[1], bottom_right[1], bottom_left[1]),
    y = c(top_left[2], top_right[2], bottom_right[2], bottom_left[2])
  )
  

  split_string <- unlist(strsplit(df_high100[i,]$time, "(?<=\\G.{2})", perl = TRUE))
  time = paste0(split_string, collapse=":")
  download_pvolfiles(date_min = as.POSIXct(paste0(df_high100[i,]$date, " ", time)), 
                     date_max=as.POSIXct(paste0(df_high100[i,]$date, " ", time)),
                     radar = df_high100[i,]$station, 
                     directory="./Data/radar_pvol")
  
  my_pvolfiles <- list.files("./Data/radar_pvol", recursive = TRUE, full.names = TRUE, pattern = df_high100[i,]$filename)
  
  ###### examine MistNet ##########
  # my_pvol <- apply_mistnet(my_pvolfiles)
  # my_scan <- get_scan(my_pvol, 0.5)
  # ppi <- project_as_ppi(my_scan, range_max = 150000)

  # pdf(paste("./Plots/MistNet_examine/", df_high100[i,]$filename,"_", df_high100[i,]$box_sum_bats, ".pdf", sep=""), width = 18, height = 6, useDingbats=FALSE)
  # 
  # p1 = plot(ppi, param = "DBZH", zlim = c(-20, 80)) + geom_polygon(square_df, mapping = aes(x, y), fill = NA, color = "black") 
  # p2 = plot(ppi, param = "WEATHER") + geom_polygon(square_df, mapping = aes(x, y), fill = NA, color = "black")
  # p3 = plot(ppi, param = "CELL") + geom_polygon(square_df, mapping = aes(x, y), fill = NA, color = "black")
  # grid.arrange(p1, p2, p3, nrow = 1)
  # 
  # dev.off()
  
  ###### examine Dual-pol ##########
  my_pvol <- read_pvolfile(my_pvolfiles)
  my_scan <- get_scan(my_pvol, 0.5)
  my_ppi <- project_as_ppi(my_scan, range_max = 150000)
  my_ppi_clean <- calculate_param(my_ppi, DBZH = ifelse(RHOHV > 0.95, NA, DBZH))
  
  pdf(paste("./Plots/Dualpol_examine/", df_high100[i,]$filename,"_", df_high100[i,]$box_sum_bats, ".pdf", sep=""), width = 18, height = 6, useDingbats=FALSE)
  p1 = plot(my_ppi, param = "DBZH", zlim = c(-20, 80)) + geom_polygon(square_df, mapping = aes(x, y), fill = NA, color = "black")
  p2 = plot(my_ppi_clean, param = "DBZH", zlim = c(-20, 80)) + geom_polygon(square_df, mapping = aes(x, y), fill = NA, color = "black")
  grid.arrange(p1, p2, nrow = 1)
  dev.off()
}

# plot one scan manually

download_pvolfiles(date_min=as.POSIXct("2022-05-05 01:25:46"), 
                   date_max=as.POSIXct("2022-05-05 01:25:46"),
                   radar = "KDFX", 
                   directory="./Data/radar_pvol")
# store the filenames in my_pvolfiles
my_pvolfiles <- list.files("./Data/radar_pvol", recursive = TRUE, full.names = TRUE, pattern = "KDFX20220505_012546_V06")
# print to console our files:
#my_pvolfiles
# let's load the first of our downloaded files:
my_pvol <- read_pvolfile(my_pvolfiles)
# let's extract the scan collected at 1.5 degree elevation from our polar volume:
my_scan <- get_scan(my_pvol, 0.5)

# before we can plot the scan, we need to project it on a Cartesian grid,
# i.e. we need to make a Plan Position Indicator (PPI)
my_ppi <- project_as_ppi(my_scan, range_max = 150000)

# Now we are ready to plot the ppi, for example let's plot reflectivity factor DBZH:
plot(my_ppi, param = "DBZH")

# only for dual-pol radar
# Screen out the reflectivity areas with RHOHV < 0.95
my_ppi_clean <- calculate_param(my_ppi, DBZH = ifelse(RHOHV > 0.95, NA, DBZH))
plot(my_ppi_clean, param = "DBZH")


# apply the MistNet model to the polar volume file and load it as a polar volume (pvol):
my_pvol <- apply_mistnet(my_pvolfiles)
# mistnet will add additional parameters to the
# elevation scans at 0.5, 1.5, 2.5, 3.5 and 4.5 degrees
# let's extract the scan closest to 0.5 degrees:
my_scan <- get_scan(my_pvol, 0.5)

# Project the scan as a ppi
ppi <- project_as_ppi(my_scan, range_max = 150000)
plot(ppi, param = "DBZH")
# Plot the MistNet class probability [0-1] for weather
plot(ppi, param = "WEATHER")
# Plot the final segmentation result, with values >1 indicating
# areas classified as weather, and value 1 pixels that fall within an
# additional 5 km fringe around weather areas
plot(ppi, param = "CELL")

