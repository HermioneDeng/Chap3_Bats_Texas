# Assuming df1 and df2 are your two data frames
# Get the columns present in both data frames
df1 = screened_df_count
df2 = first_df_nlcd

length(unique(df1$track_id))
length(unique(df2$track_id))

common_vars <- intersect(unique(df1$track_id), unique(df2$track_id))

# Get the columns present in df1 but not in df2
vars_only_in_df1 <- setdiff(unique(df1$track_id), unique(df2$track_id))

# Get the columns present in df2 but not in df1
vars_only_in_df2 <- setdiff(unique(df2$track_id), unique(df1$track_id))

# filter the rows present only in df1 or only in df2
df1_filtered = df1[df1$track_id %in% vars_only_in_df1, ]
is.na(df1_filtered$notes)

df1_filtered %>%
  filter(row_number(df1_filtered$from_sunset) <= nrow(df1_filtered)/2
         

length(unique(screened_df_count_filtered$track_id))
df[is.na(df$cluster_ID), ]

ggplot(data, aes(x=xlong, y=ylat))+
  geom_point(fill = "green", shape = 21)


##################################################################################

library(rosm)
library(ggspatial)
library(prettymapr)

ggplot(data, aes(y=ylat, x=xlong)) +
  annotation_map_tile("osm", zoom = 13) +
  geom_spatial_point() +
  annotation_scale(location = "tl", width_hint=.25) +
  annotation_north_arrow(location = "br", which_north = "true",
                         style = north_arrow_orienteering,
                         width = unit(.75, "cm"),  height = unit(1, "cm"))+
  labs(y=expression("Latitude"~(degree~"N")), x = expression("Longitude"~(degree~"W")),
       color="MSAS")+
  scale_color_viridis_c(direction = -1)

######################################################################################
filter_by_year <- function(data, year, time_column = "time", tz = "America/Chicago") {
  start_date <- as.POSIXct(paste0(year, "-01-01 00:00:00"), tz = tz)
  end_date <- as.POSIXct(paste0(year, "-12-31 23:59:59"), tz = tz)
  
  data %>%
    filter(get(time_column) >= start_date & get(time_column) <= end_date)
}

# Use the function to filter for the year 2023
filtered_file_2022 <- filter_by_year(file, 2022)

specific_hours <- c(18, 21)

filtered_file_2022 %>% filter(ID==1) %>% 
  mutate(date = date(time),
         hour = hour(time)) %>% 
  group_by(date) %>% 
  mutate(night_mean = mean(air.2m[hour %in% specific_hours], na.rm = TRUE),
         day_mean = mean(air.2m, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(date, night_mean), color="blue")+
  geom_line(aes(date, day_mean), color="orange")
  
  
  timeDatelt = as.POSIXct(file$time[1000], tz = "America/Chicago")
  str(timeDatelt)
  unclass(timeDatelt)
  
  
  
########## test NA #############
combined_df_rf[is.na(combined_df_rf$box_sum_bats), ]

  
################################################################################  
  
#### what's wrong with my env ID? ###
  
air_texas_df <- as.data.frame(air_texas, xy = TRUE, na.rm = TRUE)
roosts_sf <- st_as_sf(roosts)

# Plot air_texas raster with roost points
ggplot() +
  geom_raster(data = air_texas_df, aes(x = x, y = y, fill = air_1)) +
  scale_fill_viridis_c() + # Use a color scale (optional)
  geom_sf(data = roosts_sf, aes(geometry = geometry), color = "red", size = 1) + # Plot roost points
  geom_text(data = roosts_sf, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = cluster_ID), hjust = -0.3, vjust = -0.3) + # Add IDs
  theme_minimal() +
  labs(title = "Roost Points on air_texas Map", x = "Longitude", y = "Latitude", fill = "Temperature") +
  theme(plot.title = element_text(hjust = 0.5))

  
################################################################################
## Fit the Models and Predict Values

# Fit separate models for each year and predict values
unique_years <- unique(count.df$year)
models <- list()
predictions <- data.frame()
spring_data %>% filter(!complete.cases(.))

for (yr in unique_years) {
  data_subset <- subset(count.df, year == yr)
  model <- bam(day_sum_90q_count ~ s(yday), data = data_subset, family = poisson(), method = "REML")
  models[[as.character(yr)]] <- model
  data_subset$fitted_values <- predict(model, type = "response")
  predictions <- rbind(predictions, data_subset)
}

# Calculate phenology metrics
phenology_metrics <- data.frame(year = character(), spring_50th = numeric(), autumn_50th = numeric(), stringsAsFactors = FALSE)

for (yr in unique_years) {
  data_subset <- subset(predictions, year == yr)
  
  # Identify peaks in the fitted values
  peaks <- find_peaks(data_subset$fitted_values)
  
  if (length(peaks) >= 2) {
    # Calculate cumulative sum up to the first peak (spring) and from the last peak (autumn)
    spring_data <- data_subset[1:peaks[1], ]
    autumn_data <- data_subset[peaks[length(peaks)]:nrow(data_subset), ]
    
    spring_cumsum <- cumsum(spring_data$fitted_values)
    autumn_cumsum <- cumsum(autumn_data$fitted_values)
    
    # Find the 50th percentile date for spring and autumn
    spring_50th <- spring_data$yday[which.min(abs(spring_cumsum - 0.5 * max(spring_cumsum)))]
    autumn_50th <- autumn_data$yday[which.min(abs(autumn_cumsum - 0.5 * max(autumn_cumsum)))]
    
    phenology_metrics <- rbind(phenology_metrics, data.frame(year = as.character(yr), spring_50th = spring_50th, autumn_50th = autumn_50th))
  }
}

print(phenology_metrics)

# Plot the raw data points and fitted curves with vertical lines for phenology metrics
ggplot(predictions, aes(x = yday, y = day_sum_90q_count, color = year)) +
  geom_point(alpha = 0.4, shape = 21) +  # Raw data points
  geom_line(aes(y = fitted_values), size = 2) +  # Fitted values
  geom_vline(data = phenology_metrics, aes(xintercept = spring_50th), linetype = "dashed", color = "green", size = 1.5) +
  geom_vline(data = phenology_metrics, aes(xintercept = autumn_50th), linetype = "dashed", color = "orange", size = 1.5) +
  facet_wrap(~ year, scales = "free_y") +  # Separate plot for each year
  labs(title = "Day Sum 90q Count Over Day of Year for Each Year",
       x = "Day of Year",
       y = "Day Sum 90q Count") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("./Plots/region_gam_splines.pdf", width = 16, height = 10)

# find peak

## Identify Peaks and Calculate Percentiles

# Function to identify peaks
find_peaks <- function(x) {
  diff_sign <- diff(sign(diff(x)))
  peaks <- which(diff_sign == -2) + 1
  return(peaks)
}

# # Function to find peaks in a GAM curve
find_peaks <- function(y, x) {
  # Calculate the difference in y values
  dy <- diff(y)
  # Find where the sign of the difference changes from positive to negative (a peak)
  peaks <- which(dy[-1] < 0 & dy[-length(dy)] > 0) + 1
  # Return the x-values corresponding to the peaks
  return(x[peaks])
}

which(diff(sign(diff(df1$y)))==-2)+1

df2 = data.frame(x = data_subset$yday, y = data_subset$fitted_values)
plot(df2)
dens = df2$y
z = data.frame(dens = dens, lag = lag(dens), lead = lead(dens))
df2$x[which(with(z, dens>lag&dens>lead))]
plot(df2)

abline(v = df2$x[which(with(z, dens>lag&dens>lead))])

ggplot(df2, aes(x,y)) + geom_point() + theme_minimal()+
  geom_vline(xintercept = a[1]) +
  geom_vline(xintercept = a[2]) +
  geom_vline(xintercept = a[3])

df3 = unique(df2)
peaks <- findpeaks(df3$y, sortstr = TRUE)
top_peaks = peaks[1:6, ]
x_values = df$x[top_peaks[, 2]]

plot(df2)
abline(v = x_values, col="red")


# Calculate numerical derivative
df3$dy_dx <- c(NA, diff(df3$y) / diff(df3$x)) # NA for the first point as it has no previous point

# Derivative plot
ggplot(df3, aes(x = x, y = dy_dx)) +
  geom_line(color = "green") +
  labs(title = "Derivative of Smoothed Data",
       y = "dy/dx")

# Find indices where dy_dx changes from positive to negative
changes <- with(df3, which(diff(sign(dy_dx)) == -2))

# Extract the x positions where this change occurs
x_changes <- df3$x[changes + 1] # +1 to align with the correct x positions
x_changes
plot(df3)

order(df3$x)

#############################################################################
setdiff(count.df$cluster_ID, colony_df$cluster_ID)

colony <- c(
  "1" = "Bracken Cave",
  "15" = "Camden Street Bridge",
  "2" = "Congress Bridge",
  "0" = "McNeil Bridge",
  "11" = "D'Hanis Bridge",
  "8" = "Bamberger Ranch Preserve",
  "5" = "Old Tunnel State Park",
  "7" = "Frio Cave",
  "6" = "Davis Cave",
  "21" = "Eckert James River Cave",
  "14" = "Devil's Sinkhole",
  "9" = "Ney Cave",
  "4" = "Rucker and Stuart Cave",
  "3" = "Cluster 3",
  "18" = "Cluster 18",
  "36" = "Cluster 36",
  "10" = "Cluster 10",
  "17" = "Cluster 17",
  "16" = "Cluster 16",
  "12" = "Cluster 12",
  "13" = "Cluster 13",
  "19" = "Cluster 19",
  "22" = "Cluster 22",
  "23" = "Cluster 23",
  "24" = "Cluster 24",
  "25" = "Cluster 25",
  "27" = "Cluster 27",
  "29" = "Cluster 29",
  "33" = "Cluster 33",
  "37" = "Cluster 37",
  "38" = "Cluster 38",
  "28" = "Cluster 28",
  "26" = "Cluster 26",
  "20" = "Cluster 20",
  "42" = "Cluster 42",
  "43" = "Cluster 43"
)

roost_type <- c(
  "1" = "cave", # Bracken Cave
  "15" = "anthropogenic", # Camden Street Bridge
  "2" = "anthropogenic", # Congress Bridge
  "0" = "anthropogenic", # McNeil Bridge
  "11" = "anthropogenic", # D'Hanis Bridge
  "8" = "cave", # Bamberger built the bat cave
  "5" = "cave", # train tunnel at Old Tunnel State Park
  "7" = "cave", # Frio Cave
  "6" = "cave", # Davis Cave
  "21" = "cave", # Eckert James River Cave
  "14" = "cave", # Devil's Sinkhole
  "9" = "cave", # Ney Cave
  "4" = "cave", # Rucker and Stuart Cave
  "3" = "cave", # Huber Limestone Mine (stadium size underground) at Marble Falls
  "18" = "anthropogenic", # SW H K Dodgen loop 363, Temple, TX
  "36" = "anthropogenic", #Texas A&M (at College Station) Stadium
  "10" = "anthropogenic", #San Angelo, Forest Road overpass at Loop 306
  "17" = "cave", # Devil's River State Natural Area, "natural"
  "16" = "anthropogenic", # near Dabney and W US Hight 90
  "12" = "anthropogenic", # Eagle Pass (Mexico boarder)
  "13" = "anthropogenic", #N US Highway 183
  "19" = "anthropogenic", # near Poth, TX, cannot confirm but probably crevices
  "22" = "cave", # Interstate 10 W outside San Antonio
  "23" = "anthropogenic", #Cypress Creek Bridge, Wimberley
  "24" = "anthropogenic", # Thorndale
  "25" = "anthropogenic", # E US Highway 90 & Johnstone
  "27" = "anthropogenic", #San Marcos I 35N interstate or river bridges
  "29" = "anthropogenic", # near Milano & US Highwat 190
  "33" = "anthropogenic", # near Lockhart & State Highway 130
  "37" = "anthropogenic", # N US Highway 277
  "38" = "anthropogenic", # Sequin
  "28" = "anthropogenic", #Ballinger
  "26" = "anthropogenic", #Paint Rock
  "20" = "anthropogenic", #US Highway 87 E
  "42" = "anthropogenic", #Eden
  "43" = "anthropogenic" #Menard
)

colony_df <- data.frame(cluster_ID = names(colony), colony = unname(colony), 
                        roost_type = unname(roost_type),
                        stringsAsFactors = FALSE)
write.csv(colony_df, "./Data/colony_info.csv", row.names = F)

c = count.df %>% group_by(cluster_ID) %>% 
  summarize(detections_num = n_distinct(track_id))

colony_df$cluster_ID = as.factor(colony_df$cluster_ID)
colony_df = left_join(colony_df, c, by="cluster_ID")

##############################################################################

# Step 2: Create an irregular time series using the zoo package
# Zoo works well with irregular dates and missing periods
daily_regional_ts <- zoo(daily_regional_data$day_sum_90q_count,
                         order.by = daily_regional_data$local_date)

# Plot to check
plot(daily_regional_ts, main = "Daily Regional Bat Count", ylab = "Count", xlab = "Date")

# Step 3: Decompose the time series
# Since STL decomposition requires regular data, you can fill in the missing dates with NAs and then apply STL
# Create a regular sequence of dates from the minimum to maximum date
full_date_seq <- seq(min(daily_regional_data$local_date), max(daily_regional_data$local_date), by = "day")

# Merge the full date sequence with the original data, filling in missing dates with NAs
daily_regional_ts_full <- merge(daily_regional_ts, zoo( ,full_date_seq), all = TRUE)

# Convert the irregular time series to a regular one by filling the gaps with NAs
daily_regional_ts_regular <- na.approx(daily_regional_ts_full, na.rm = FALSE)

# Convert the regular zoo series to a ts object for STL decomposition
daily_regional_ts_for_stl <- ts(coredata(daily_regional_ts_regular),
                                frequency = 365,
                                start = c(year(min(full_date_seq)), yday(min(full_date_seq))))

# Step 4: Apply STL decomposition
stl_decomp <- stl(daily_regional_ts_for_stl, s.window = "periodic")

# Extract the trend component
trend <- stl_decomp$time.series[, "trend"]

# Plot the decomposition
plot(stl_decomp)

# Step 5: Bootstrapping to estimate confidence intervals for trends
# Define a function to get the trend component
get_trend <- function(data, indices) {
  boot_data <- data[indices]
  ts_data <- ts(boot_data, frequency = 365)
  stl_res <- stl(ts_data, s.window = "periodic")
  return(stl_res$time.series[, "trend"])
}

# Run the bootstrapping (1000 replications)
boot_res <- boot(daily_regional_ts_for_stl, statistic = get_trend, R = 1000)

# Calculate 95% confidence intervals for the trend component
boot_ci_lower <- apply(boot_res$t, 2, quantile, probs = 0.025)
boot_ci_upper <- apply(boot_res$t, 2, quantile, probs = 0.975)

# Plot the trend with bootstrapped confidence intervals using ggplot
# Prepare a data frame for ggplot
plot_data <- data.frame(
  Date = full_date_seq,
  Trend = as.numeric(trend),
  LowerCI = boot_ci_lower,
  UpperCI = boot_ci_upper
)

# Create the ggplot
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Trend), color = "blue", size = 1) +  # Plot the trend
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), alpha = 0.2, fill = "red") +  # Confidence intervals
  labs(title = "Trend with Bootstrapped Confidence Intervals",
       x = "Date", y = "Regional Daily Bat Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


###################################################
# Z-Score method to filter out outliers based on one column
filter_zscore <- function(df, column_name, threshold = 3) {
  z_scores <- abs((df[[column_name]] - mean(df[[column_name]], na.rm = TRUE)) / sd(df[[column_name]], na.rm = TRUE))
  df_filtered <- df[z_scores < threshold, ]
  return(df_filtered)
}

# Example usage
df.filtered = count.df_from2006 %>% 
  filter(colony == "Bracken Cave") %>% 
  group_by(year) %>% 
  group_modify(~ filter_zscore(.x, "track_90q_count")) %>% # group_modify allows you to apply the function per group
  ungroup()

ggplot(df.filtered, aes(x = fake_date, y = track_90q_count/1000000, group=year, color = year)) +
  geom_point()+
  geom_line()+
  scale_x_date(date_breaks = "months", date_labels = "%b")+
  labs(x="Month", y="Count (millions)")+
  facet_wrap(~year, ncol = 3)+
  theme_light()

# Modify filter_zscore to return outliers' track_ids
find_outliers_zscore <- function(df, column_name, threshold = 3) {
  z_scores <- abs((df[[column_name]] - mean(df[[column_name]], na.rm = TRUE)) / sd(df[[column_name]], na.rm = TRUE))
  
  # Return rows where Z-score exceeds the threshold
  outliers <- df[z_scores >= threshold, ]
  
  return(outliers$track_id)  # Return only the track_id for the outliers
}

# Corrected pipeline to extract track_id of outliers
count.df_from2006 %>%
  filter(colony == "Marble Falls") %>%
  group_by(year) %>%
  group_modify(~ tibble(track_id = find_outliers_zscore(.x, "track_90q_count"))) %>%  # Collect outlier track_ids
  unique()%>% filter(year==2016)


##################################################################################################################

# Get the posterior predictions (population estimates) for each year
year_predictions <- posterior_predict(fit, newdata = new_data)
# Calculate mean and 95% credible intervals for each row of new_data
summary_stats <- apply(year_predictions, 2, function(x) {
  mean_val <- mean(x)
  ci_lower <- quantile(x, 0.025)
  ci_upper <- quantile(x, 0.975)
  return(c(mean = mean_val, lower = ci_lower, upper = ci_upper))
})
summary_stats <- t(summary_stats)
new_data = cbind(new_data, summary_stats)

# Convert posterior predictions to long format for ridgeline plots
predictions_long <- as.data.frame(year_predictions)
names(predictions_long) = unique(roost.df$year)
predictions_long$iteration <- 1:nrow(predictions_long)

predictions_long <- predictions_long %>%
  tidyr::gather(key = "year_yday", value = "population_estimate", -iteration)

# Convert year column back to numeric if necessary
predictions_long$year <- as.numeric(predictions_long$year)


# Compute mean and 95% CI for each year
year_summary <- apply(year_predictions, 2, function(x) {
  c(mean = mean(x),
    lower = quantile(x, 0.025),
    upper = quantile(x, 0.975))
})

# Convert to a dataframe
year_summary_df <- as.data.frame(t(year_summary))
year_summary_df$year <- unique(roost.df$year)
names(year_summary_df)[2:3] = c("lower", "upper")

# Plot with points and 95% CI bars
# ggplot(year_summary_df, aes(x = year, y = mean)) +
#   geom_point(size = 3) +   # Points for the population estimate
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # Error bars for 95% CI
#   labs(title = "Estimated Bat Population Size with 95% CI",
#        x = "Year", y = "Estimated Population Size") +
#   dark_theme_minimal()

# Final estimated population and 95% CI averaged across all years
# Flatten the year_predictions matrix into a single vector
all_year_predictions <- as.vector(year_predictions)

# Compute overall mean and 95% CI
overall_mean <- mean(all_year_predictions)
overall_ci <- quantile(all_year_predictions, c(0.025, 0.975))

colony_size_posterior = rbind(colony_size_posterior, 
                              data.frame(colony = c, overall_mean, overall_ci) %>%
                                tibble::rownames_to_column(var = "CI_Level") %>%
                                pivot_wider(names_from = CI_Level, 
                                            values_from = overall_ci, names_prefix = "CI"))

# Ridgeline plot of posterior distributions for each year
ggplot() +
  geom_density_ridges(predictions_long, mapping=aes(x = population_estimate, y = factor(year), fill = year, height = ..density..), stat = "density", scale = 1, rel_min_height = 0.01) +
  geom_point(year_summary_df, mapping=aes(x = mean, y = factor(year)), color = "black")+
  labs(title = paste0(c, " Posterior Distributions of Yearly Population Estimates"),
       x = "Population Estimate", y = "Year") +
  theme_minimal() +
  scale_fill_viridis()+
  theme(axis.text.y = element_text(size = 10),  # Adjust y-axis text size
        axis.title.y = element_text(size = 12),
        legend.position = "none")+ # Adjust y-axis title size
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6), limits = c(0, 5000000))
ggsave(paste("./Plots/brms_pop_est/Ridgeline_", c, ".pdf", sep=""), width = 6, height = 6, useDingbats=FALSE)

##################################################################################################################

### estimate posterior annual abundance per colony
```{r}
# Define the model formula for estimating annual abundance
# Use 'year' and 'colony' as grouping factors, with a negative binomial family
annual_abundance_model <- brm(
  as.integer(track_90q_count) ~ year + (1 | colony) + (1 | year:colony),  # Random intercepts for colonies and year:colony interaction
  data = count.df.brms,  # Replace with your dataset
  family = negbinomial(),  # Specify Negative Binomial distribution
  chains = 4,              # Number of MCMC chains
  cores = 4,               # Number of cores to use
  iter = 6000,             # Number of iterations (increase if necessary)
  warmup = 2000,           # Warmup period
  control = list(adapt_delta = 0.95,     # Increase adapt_delta to reduce divergences (default is 0.8)
                 max_treedepth = 15)      # Increase max_treedepth from default 10 to 15)
)

# Print the model summary
summary(annual_abundance_model)
plot(annual_abundance_model)

# Extract posterior estimates for annual abundance for each colony
# Create a new dataset for predictions, including all years and colonies
newdata_annual <- expand.grid(year = unique(count.df.brms$year), 
                              colony = unique(count.df.brms$colony))

# Use posterior_predict to get posterior abundance estimates
annual_abundance_post <- posterior_predict(annual_abundance_model, newdata = newdata_annual)

# View summary of posterior predictions
summary(annual_abundance_post)

```

```{r}
colony_abundance_model <- brm(
  as.integer(track_90q_count) ~ year + (1 + year | colony) + (1 | month), 
  data = count.df.brms, 
  family = negbinomial(),  
  chains = 4,              # Number of MCMC chains
  cores = 4,               # Number of cores to use
  iter = 6000,             # Number of iterations (increase if necessary)
  warmup = 2000,           # Warmup period
  control = list(adapt_delta = 0.95,     # Increase adapt_delta to reduce divergences (default is 0.8)
                 max_treedepth = 15)      # Increase max_treedepth from default 10 to 15)
)
```


### Month and roost type

```{r}
lmermodel = glmer(
  formula = as.integer(track_90q_count) ~ month + (1 | colony),
  family = "poisson",  # Using Poisson family for counts
  data = count.df.brms
)
summary(lmermodel)

# Create a new dataset with combinations of month and roost_type for predictions
new_data <- expand.grid(
  month = unique(count.df.brms$month),
  roost_type = unique(count.df.brms$roost_type),
  colony = unique(count.df.brms$colony),
  yday = unique(count.df.brms$yday)
)

# Get predicted counts for new data
predicted_counts <- predict(fit_lme4, newdata = new_data, type = "response", re.form = NULL)

# Combine predictions with new_data
total_population_by_roost <- data.frame(new_data, predicted_counts) %>%
  group_by(month, roost_type) %>%
  summarize(total_population_estimate = mean(predicted_counts, na.rm = TRUE))
```

```{r}
fit_pop_colony <- brm(as.integer(track_90q_count) ~ month * roost_type + (1 | colony) + (1 | yday) + (1 | month:colony), 
                      data = count.df.brms, 
                      family = negbinomial(),
                      chains = 4, iter = 4000, warmup = 2000, cores = 8,
                      control = list(adapt_delta = 0.95, max_treedepth = 15))


summary(fit_pop_colony)
plot(fit_pop_colony)

# Generate posterior predictions for each colony and month
posterior_predictions <- posterior_epred(fit_pop_colony, newdata = count.df.brms, allow_new_levels = T)

# Calculate the mean predicted count for each colony and month
monthly_estimates <- count.df.brms %>%
  group_by(colony, month) %>%
  summarize(monthly_count_estimate = mean(posterior_predictions))

# View the monthly colony estimates
print(monthly_estimates)
```

############
# Create a new dataset with month and colony combinations for prediction
newdata <- expand.grid(month = unique(count.df.brms$month), 
                       colony = unique(count.df.brms$colony),
                       yday = 200) %>% 
  left_join(count.df.brms[c("colony", "roost_type")], multiple = "first")

# Generate posterior predictions for monthly abundance in each colony
monthly_predictions <- posterior_predict(fit_pop_colony, newdata = newdata, allow_new_levels = TRUE)

# Summarize the predictions for each month and colony
monthly_abundance <- newdata %>%
  mutate(predicted_population = colMeans(monthly_predictions)) %>%
  group_by(colony, month) %>%
  summarize(mean_pred_population = mean(predicted_population),
            lower_pred_population = quantile(predicted_population, 0.025),
            upper_pred_population = quantile(predicted_population, 0.975))

###########


```{r}
fit <- brm(as.integer(day_sum_90q_count) ~ year + (1 | yday), 
           data = daily_regional_data, 
           family = negbinomial(), 
           chains = 4, iter = 4000, warmup = 2000, cores = 8,
           control = list(adapt_delta = 0.95, max_treedepth = 15)))

# # Find the yday with the highest count for each year
# max_yday_per_year <- daily_regional_data %>%
#   group_by(year) %>%
#   summarize(yday = yday[which.max(day_sum_90q_count)])  # Get yday where count is highest

# Create the new dataset for predictions
# new_data <- data.frame(year = max_yday_per_year$year, yday = max_yday_per_year$yday)
new_data <- data.frame(year = unique(daily_regional_data$year), yday = 120) # spring population
new_data <- data.frame(year = unique(daily_regional_data$year), yday = 200) # summer population
new_data <- data.frame(year = unique(daily_regional_data$year), yday = 270) # fall population
new_data <- data.frame(year = unique(daily_regional_data$year), yday = 350) # winter population

# Get the posterior predictions (population estimates) for each year
year_predictions <- posterior_predict(fit, newdata = new_data)

# Convert posterior predictions to long format for ridgeline plots
predictions_long <- as.data.frame(year_predictions)
names(predictions_long) = unique(daily_regional_data$year)
predictions_long$iteration <- 1:nrow(predictions_long)

predictions_long <- predictions_long %>%
  tidyr::gather(key = "year", value = "population_estimate", -iteration)

# Convert year column back to numeric if necessary
predictions_long$year <- as.numeric(predictions_long$year)

##############################################################################
# Compute mean and 95% CI for each year
year_summary <- apply(year_predictions, 2, function(x) {
  c(mean = mean(x),
    lower = quantile(x, 0.025),
    upper = quantile(x, 0.975))
})

# Convert to a dataframe
year_summary_df <- as.data.frame(t(year_summary))
year_summary_df$year <- unique(daily_regional_data$year)
names(year_summary_df)[2:3] = c("lower", "upper")

# Final estimated population and 95% CI averaged across all years
# Flatten the year_predictions matrix into a single vector
all_year_predictions <- as.vector(year_predictions)

# Compute overall mean and 95% CI
overall_mean <- mean(all_year_predictions)
overall_ci <- quantile(all_year_predictions, c(0.025, 0.975))

# Ridgeline plot of posterior distributions for each year
ggplot() +
  geom_density_ridges(predictions_long, mapping=aes(x = population_estimate, y = factor(year), fill = year, height = ..density..), stat = "density", scale = 1, rel_min_height = 0.01) +
  geom_point(year_summary_df, mapping=aes(x = mean, y = factor(year)), color = "black")+
  labs(title = paste0("Regional Fall Relative Abundance Estimates (doy = 270)"),
       x = "Population Estimate", y = "Year") +
  theme_minimal() +
  scale_fill_viridis()+
  theme(axis.text.y = element_text(size = 10),  # Adjust y-axis text size
        axis.title.y = element_text(size = 12),
        legend.position = "none")+ # Adjust y-axis title size
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6), limits = c(0, 20000000))
ggsave("./Plots/regional_brms_pop_est.pdf", width = 8, height = 8, useDingbats=FALSE)

###############
# Step 1: Extract random effects for 'yday'
random_effects_yday <- ranef(fit)$yday[, , 1]  # Extracting random intercepts for yday

# Step 2: Extract the data used for 'yday' and combine with random effects
yday_data <- as.data.frame(random_effects_yday)
yday_data$yday <- as.numeric(rownames(yday_data))

# Step 3: Plot the random effects of 'yday' using ggplot2
ggplot(yday_data, aes(x = yday, y = Estimate)) +
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2, color = "grey", alpha = 0.5) +
  geom_point(color = "black", size = 2) +
  labs(title = "Random Effect of Day of Year (yday) on Bat Counts",
       x = "Day of Year (yday)",
       y = "Estimated Random Effect") +
  theme_minimal()

#################
# Step 1: Generate predictions from the model for each 'yday'
# We need to generate new data for the 'yday' and use the `predict` function
yday_new_data <- data.frame(
  year = 2016,  # Use the first year for prediction
  yday = seq(1, 365, by = 1)
)

# Step 2: Generate predicted counts based on the model
predicted_counts <- posterior_predict(fit, newdata = yday_new_data, summary = TRUE, allow_new_levels = TRUE)

# Step 3: Prepare data for plotting
# Combine the predictions with the yday values
plot_data <- data.frame(
  yday = yday_new_data$yday,
  predicted_count = apply(predicted_counts, 2, mean),  # Mean predicted count
  lower_ci = apply(predicted_counts, 2, quantile, probs = 0.025),  # 2.5% quantile for CI
  upper_ci = apply(predicted_counts, 2, quantile, probs = 0.975)   # 97.5% quantile for CI
)

# Step 4: Plot the predicted counts with confidence intervals using ggplot2
ggplot(plot_data, aes(x = yday, y = predicted_count)) +
  geom_line(color = "blue", size = 1) +  # Plot the predicted counts
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2, fill = "red") +  # Confidence intervals
  labs(title = "Predicted Bat Counts Based on Day of Year (yday)",
       x = "Day of Year (yday)",
       y = "Predicted Bat Count") +
  theme_minimal()+
  scale_y_continuous(limits = c(0, 20000000), labels = unit_format(unit = "M", scale = 1e-6))
```


```{r}
# night variables
# remove s(lon, lat) due to high concurvity with s(dist)
bam5.1 <- bam(log(adjusted_VIC) ~ s(lunar.illumination) + s(air.sfc.night) + s(prate.night) + s(pres.sfc.night) +  s(rhum.2m.night) + s(tcdc.night) + s(uwind.10m.night) + s(vwind.10m.night) + s(avg_drought_index) + s(from_sunset) + s(yday) + s(geo_dist) + roost_type, discrete=TRUE, select=TRUE, nthreads=1, method = 'fREML', data = combined_df)

summary(bam5.1) # Deviance explained = 42.1%

s5.1 = getViz(bam5.1)

print(plot(s5.1, trans = function(x){exp(coef(bam5.1)[1] + x)})+dark_theme_classic(), page = 1)
plot(bam5.1, trans = function(x){exp(coef(bam5.1)[1] + x)}, pages = 1)


plot(sm(s5.1,12), trans = function(x){exp(coef(bam5.1)[1] + x)}, seWithMean = TRUE)+
  l_fitLine() + l_ciLine() + l_rug(alpha = 0.4) + ggtitle("G")+ dark_theme_classic()+
  xlab("Distance from radar (m)") + ylab("count")+theme(plot.title = element_text(face = "bold"))+ scale_y_continuous(limits=c(0, 120000))

concurvity(bam5.1, full=T)
concurvity(bam5.1, full=F)
```

