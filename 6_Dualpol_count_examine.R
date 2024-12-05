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

# texas_bats_v3 (png) & texas_bats_v3_more (jpg)
# Stations: KDFX, KEWX, KSJT, KGRK
# Years: 1994-2023
# 2000-2020 in texas_bats_v3
# other years in texas_bats_v3_more due to being deployed later
# All days for each year
# 90 min before to 150 min after local sunset
# rcs = 4.519

# Counting animals
# sweep: not counting a pixel as bats if beyond 21630891 (60dbZ)
# used read_s3, which causes some NA counts
# weather contaminated pixels caused inaccurate counts
# sweep2: not counting a pixel as bats if beyond 216309 (40dbZ)
# switched to read_http
# using a lower threshold, still exists weather contamination
# sweep3: see “Documentation for roost-system / Deployment and Screening / Counting”
# use dualpol when available

# Count from the radar object but not the rendered and interpolated array
# Config
# Scale the box by 1.2x to be a bounding box
# max height 5000m
# rcs = 1
# dual-pol cross correlation filtering if available; three counts -- no dBZ filtering, filter at 60dBZ, filter at 40dBZ
# 
# sweeps*.txt
# track_id,
# filename,
# sweep_idx,
# sweep_angle,
# count_scaling,  # enlarge each bounding box by 1.2x
# # number of pixels in the bounding box, NaN → -33dBZ
# n_roost_pixels,
# # filter by dual-pol cross-correlation when available and requested
# # count the high and NaN cross correlation pixels
# n_weather_pixels,
# # filter by dBZ when requested
# # among the kept pixels, count the number of high dBZ pixels
# n_highZ_pixels_60,
# n_highZ_pixels_40,
# # finally count animals in the remaining pixels
# n_animals,  # no filtering by dBZ
# n_animals_60,  # keep pixels with lower than 60dBZ (21630891)
# n_animals_40  # keep pixels with lower than 40dBZ (216309)



library(fs)

sweep_path = "./Data/counts_dualpol/sweeps/"
track_path = "./Data/counts_dualpol/tracks/"
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
      mutate(weather_ratio = n_weather_pixels/n_roost_pixels,
                box_sum_bats = sum(n_animals),
                box_sum_bats_60 = sum(n_animals_60),
                box_sum_bats_40 = sum(n_animals_40)) %>% 
      filter(weather_ratio<0.5)
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