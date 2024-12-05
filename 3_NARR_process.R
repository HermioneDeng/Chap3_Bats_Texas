library(terra)
library(sf)
library(dplyr)
library(fs)
library(stringr)
library(tictoc) 

path = "../NARR"

# us = vect('./Data/US_State_Boundaries/US_State_Boundaries.shp')
# texas = us[us$NAME == "Texas"]
texas = vect("./Data/SC_Texas/SC_Texas.shp")

# Create roost buffers to extract env vairables based on cluster center
cluster_df_NOduplicate = read.csv("./Data/4_firstdetections_unduplicated.csv")
data <- unique(cluster_df_NOduplicate[c("cluster_ID", "ctr_lon", "ctr_lat")])
data_sf <- st_as_sf(data, coords = c("ctr_lon", "ctr_lat"), crs = 6933)
data_sf <- st_transform(data_sf, crs = 4326)
roosts = vect(st_buffer(data_sf, dist = 15000)) # 15 km buffers for all
roosts$cluster_ID <- as.factor(roosts$cluster_ID)  # Ensure cluster_ID is a factor or character SO IMPORTANT!!!!!!
writeVector(roosts, "./Data/roosts_center_buffer.shp", overwrite=TRUE)

## Start extraction

for (variable_file in dir_ls(path)){                            #each variable
  name = str_sub(variable_file, start = 9) #change start according to path
  air_2m_allyears = data.frame()
  
  for (file in dir_ls(variable_file)){                          #each year 2000-2022 for one variable
  tic()
  print(file)
    
  air = rast(file)
  air_2m = data.frame()
  
    for (i in 1:dim(air)[3]){                             #each day within a year
    air_ll = project(air[[i]],"EPSG:4326")
    air_texas = terra::mask(crop(air_ll, texas),texas)
    ## Extract 3-hourly temperature measurements to each roost point
    air_points = terra::extract(air_texas, roosts, fun=mean, bind = TRUE)
    air_points <- as.data.frame(air_points)
    names(air_points)[2] = name
    air_points$time = time(air_ll)
    air_2m = rbind(air_2m, air_points)
    }
  
  air_2m_allyears = rbind(air_2m_allyears, air_2m)
  toc()
  
  write.csv(air_2m_allyears, paste0("./Data/env/", name, "_2000_to_2023.csv"))
  
  }
  }
  
  




################################################################################

# Learning more detailed netCDF processing: https://pjbartlein.github.io/REarthSysSci/netCDF.html#data-frame-to-array-conversionrectangular-to-raster

#### 2 Reading a netCDF data set using the ncdf4 package

# Install and load required packages
library(ncdf4)

### 2.1 Open the NetCDF file

# set path and filename
ncpath <- "./Data/env/"
ncname <- "air.2m.2022"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "air"  # note: tmp means temperature (not temporary)

ncin <- nc_open(ncfname)
ncin

### 2.2 Get coordinate (including time) variables

# get longitude and latitude
y <- ncvar_get(ncin,"y") #lat
ny <- dim(y)
x <- ncvar_get(ncin,"x") #lon
nx <- dim(x)
print(c(ny,nx))

lat <- ncvar_get(ncin,"lat") #lat
lon <- ncvar_get(ncin,"lon") #lon

# get time
time <- ncvar_get(ncin,"time")
time
tunits <- ncatt_get(ncin,"time","units")
tunits
nt <- dim(time)
nt

### 2.3 Get a variable

# get temperature
tmp_array <- ncvar_get(ncin, "air")
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

nc_close(ncin)
ls()


#### 3. Reshaping from raster to rectangular

library(lattice)
library(RColorBrewer)
library(CFtime)
library(terra)

### 3.1 Convert the time variable

# decode time ( converts time as stored as “time-since some origin”)
cf <- CFtime(tunits$value, calendar = "proleptic_gregorian", time) # convert time to CFtime class
cf

timestamps <- CFtimestamp(cf) # get character-string times
timestamps

time_cf <- CFparse(cf, timestamps) # parse the string into date components
time_cf$timestamps = timestamps
time_cf

### 3.2 Replace netCDF fillvalues with R NAs

# replace netCDF fill values with NA's
tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))

### 3.3 Get a single time slice of the data, create an R data frame, and write a .csv file

# get a single slice or layer
dim(tmp_array)
m <- 1460
tmp_slice <- tmp_array[,,m]

# Turn the array into a raster
tmp_slice_r = rast(tmp_slice)

# quick map
image(x,y,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# levelplot of the slice
grid <- expand.grid(lon=x, lat=y)
cutpts <- seq(260, 320, by=5)
levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=13, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))

# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
xy <- as.matrix(expand.grid(x, y))
dim(xy)

# vector of `tmp` values
tmp_vec <- as.vector(tmp_slice)
length(tmp_vec)

# create dataframe and add names
tmp_df01 <- data.frame(cbind(xy,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
head(na.omit(tmp_df01), 10)

# set path and filename
csvpath <- "./Data/env/"
csvname <- "cru_tmp_1.csv"
csvfile <- paste(csvpath, csvname, sep="")
write.table(na.omit(tmp_df01),csvfile, row.names=FALSE, sep=",")

## 3.4.1 Reshape the whole array

# reshape the array into vector
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

# reshape the vector into a matrix
tmp_mat <- matrix(tmp_vec_long, nrow=nx*ny, ncol=nt)
dim(tmp_mat)

# create a dataframe
lonlat <- as.matrix(expand.grid(x,y))
tmp_df02 <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df02) <- c("lon","lat", timestamps)

##################################################################

