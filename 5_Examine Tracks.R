##############################################################################
# Examining high counts (need to figure out a way to eliminate weather contaminated scans, still a lot)
##############################################################################

library(bioRad)
library(vol2birdR)

df_high = filter(count.df, track_90q_count>3000000 & track_90q_count<=4000000)
#df_high = count.df %>% filter(filename=="KEWX20080622_015758") %>% filter(track_id == "KEWX20080621-41")

image2xy = function(x, y, r, dim=600, rmax=150000){
  
  x0 = y0 = dim/2.0 # origin
  x =  (x - x0)*2*rmax/dim
  y = -(y - y0)*2*rmax/dim
  r = r*2*rmax/dim*1.2
  return(list(x = x, y = y, r = r))
}


for (i in 1:nrow(df_high)){
  
  # DRAW a box around the roost
  l = image2xy(df_high[i,]$x, df_high[i,]$y, df_high[i,]$r)
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
  
  
  split_string <- unlist(strsplit(substr(df_high[i,]$filename, 14, 19), "(?<=\\G.{2})", perl = TRUE))
  time = paste0(split_string, collapse=":")
  formatted_date <- as.Date(as.character(df_high[i,]$date), format = "%Y%m%d")
  
  # Combine the formatted date and time
  datetime_str <- paste0(formatted_date, " ", time)
  
  # Convert to POSIXct
  datetime <- as.POSIXct(datetime_str, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  
  download_pvolfiles(date_min = datetime, 
                     date_max = datetime,
                     radar = df_high[i,]$station, 
                     directory="./Data/radar_pvol")
  
  my_pvolfiles <- list.files("./Data/radar_pvol", recursive = TRUE, full.names = TRUE, pattern = df_high[i,]$filename)
  
  my_pvol <- read_pvolfile(my_pvolfiles)
  my_scan <- get_scan(my_pvol, 0.5)
  my_ppi <- project_as_ppi(my_scan, range_max = 150000)
  
  pdf(paste("./Plots/High_count_examine/", df_high[i,]$filename_det_idx, ".pdf", sep=""), width = 6, height = 6, useDingbats=FALSE)
  
  print(plot(my_ppi, param = "DBZH", zlim = c(-20, 80)) + 
          geom_polygon(square_df, mapping = aes(x, y), fill = NA, color = "black") + 
          ggtitle(df_high[i,]$box_sum_bats) +
          theme_void())
  
  dev.off()
  
}


##############################################################################
# Examining count within tracks (the right way to calculate number per track)
##############################################################################

lapply(c("dplyr", "ggplot2"), library, character.only = TRUE)

# During the Bracken cave visit, bats emerge with several waves 
# (common during parturition and pup flight stages, along with changes of timing)

count.df = read.csv("./Data/8.5_count.df.csv") %>% 
  group_by(track_id) %>% 
  mutate(final.length = n_distinct(from_sunset))

count.ex = count.df %>% filter(final.length>5)

pdf("./Plots/track_examine.pdf", width = 5, height = 3)
for (i in unique(count.ex$track_id)){
  p = ggplot(data = count.df %>% filter (track_id == i), 
         mapping = aes(x = from_sunset, y = box_sum_bats)) + 
    geom_point()+
    geom_line() + 
    xlab("Time From Sunset") +  
    ylab("# of Bats")
  print(p)
}
dev.off()


#######################################################################
# Create a composite of multiple plan position indicators (ppi)
#######################################################################

list = c("KEWX20230716_013309_V06", "KDFX20230716_015250_V06", "KGRK20230716_020518_V06", "KSJT20230716_014735_V06")

i = list[4]
split_string <- unlist(strsplit(substr(i, 14, 19), "(?<=\\G.{2})", perl = TRUE))
time = paste0(split_string, collapse=":")
formatted_date <- as.Date(as.character(substr(i, 5, 12)), format = "%Y%m%d")

# Combine the formatted date and time
datetime_str <- paste0(formatted_date, " ", time)
station = substr(i, 1, 4)
datetime <- as.POSIXct(datetime_str, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


download_pvolfiles(date_min = datetime, 
                   date_max = datetime,
                   radar = station, 
                   directory="./Data/radar_pvol")

my_pvolfiles <- list.files("./Data/radar_pvol", recursive = TRUE, full.names = TRUE, pattern = i)

my_pvol <- read_pvolfile(my_pvolfiles)
my_scan <- get_scan(my_pvol, 0.5)
ppi4 <- project_as_ppi(my_scan, range_max = 150000)

composite = composite_ppi(x=list(ppi1, ppi2, ppi3, ppi4), res = 1000)

saveRDS(composite, "./Data/composite_radar.rds")

plot(composite, zlim = c(-20, 40))+ 
  labs(tag = "A")+
  theme_void()+
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         style = north_arrow_fancy_orienteering)

ggsave("./Plots/composite_radars.pdf", width = 9, height = 7)

library(bioRad)
basemap <- rosm::osm.types(zoom = 7)[1]
# basemap = download_basemap(my_composite, maptype = "toner-background")
m1 = bioRad::map(composite, map = basemap, param = "DBZH", palette = viridis::viridis(100), zlim=c(-20,40), alpha = 0.9)+ 
  labs(tag = "A")+
  theme_void()+
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         style = north_arrow_fancy_orienteering)

ggsave("./Plots/publication/composite_radars.pdf", width = 9, height = 7)

##############################################################################
# Examining one emergence event (time-series)
##############################################################################

library(bioRad)
library(vol2birdR)

# use weather climate tool to figure out the scan filenames we are going to load
temp_dir <- paste0("./Data/bioRad_tmp_files")
dir.create(temp_dir)
download_pvolfiles(
  date_min = as.POSIXct("2024-06-23 01:20", tz = "UTC"),
  date_max = as.POSIXct("2024-06-23 02:22", tz = "UTC"),
  radar = "KEWX",
  directory = temp_dir,
  overwrite = TRUE
)

files = list.files(temp_dir, recursive = TRUE, full.names = TRUE)

for (my_pvolfiles in files){
  my_pvol <- read_pvolfile(my_pvolfiles)
  my_scan <- get_scan(my_pvol, 0.5)
  my_ppi <- project_as_ppi(my_scan, range_max = 100000)
  name = substr(my_pvolfiles, 41, 70)
  pdf(paste("./Chap4_Plots/", name, ".pdf", sep=""), width = 6, height = 6, useDingbats=FALSE)
  print(plot(my_ppi, param = "DBZH") + 
          scale_y_continuous(breaks = seq(-20, 60, 10))+
          scale_fill_viridis(limits = c(-10, 40))+
          theme_void())
  dev.off()
}



