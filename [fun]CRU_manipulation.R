
CRU_manipulation <- function(sun, hum, wind, elev){

## we start with sunshine

unzip_sun <- gzfile(sun,"rt")  
data_sun <- read.table(unzip_sun, header = F) 

colnames(data_sun) <- c("Lat", "Long", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Data are in percent, maximum seems to be around 11H per day
data_sun[, 3:14] <- (data_sun[, 3:14]/100)*11
frame_sun <- data.frame(data_sun)


Sunshine <- st_as_sf(frame_sun, coords = c("Long", "Lat"), crs = "EPSG:4326")

# Create a basic raster that will gather the different attributes

Raster <- raster(crs = "EPSG:4326", vals = 0, res = c(0.5, 0.5), ext = extent(c(-180, 180, -90, 90)))

RasterJansun <- rasterize(st_as_sf(Sunshine[, 1]), Raster, field = 'Jan', fun = mean, na.rm = T)
RasterFebsun <- rasterize(st_as_sf(Sunshine[, 2]), Raster, field = 'Feb', fun = mean, na.rm = T)
RasterMarsun <- rasterize(st_as_sf(Sunshine[, 3]), Raster, field = 'Mar', fun = mean, na.rm = T)
RasterAprsun <- rasterize(st_as_sf(Sunshine[, 4]), Raster, field = 'Apr', fun = mean, na.rm = T)
RasterMaysun <- rasterize(st_as_sf(Sunshine[, 5]), Raster, field = 'May', fun = mean, na.rm = T)
RasterJunsun <- rasterize(st_as_sf(Sunshine[, 6]), Raster, field = 'Jun', fun = mean, na.rm = T)
RasterJulsun <- rasterize(st_as_sf(Sunshine[, 7]), Raster, field = 'Jul', fun = mean, na.rm = T)
RasterAugsun <- rasterize(st_as_sf(Sunshine[, 8]), Raster, field = 'Aug', fun = mean, na.rm = T)
RasterSepsun <- rasterize(st_as_sf(Sunshine[, 9]), Raster, field = 'Sep', fun = mean, na.rm = T)
RasterOctsun <- rasterize(st_as_sf(Sunshine[, 10]), Raster, field = 'Oct', fun = mean, na.rm = T)
RasterNovsun <- rasterize(st_as_sf(Sunshine[, 11]), Raster, field = 'Nov', fun = mean, na.rm = T)
RasterDecsun <- rasterize(st_as_sf(Sunshine[, 12]), Raster, field = 'Dec', fun = mean, na.rm = T)

mean_sunshine <- (RasterJansun + RasterFebsun + RasterMarsun + RasterAprsun + RasterMaysun + RasterJunsun + RasterJulsun + RasterAugsun + RasterSepsun + RasterOctsun + RasterNovsun + RasterDecsun)/12

## Humidity
unzip_hum <- gzfile(hum,"rt")  
data_hum <- read.table(unzip_hum, header = F) 

colnames(data_hum) <- c("Lat", "Long", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
frame_hum <- data.frame(data_hum)
frame_hum$Latitude <- frame_hum$Lat

humidity <- st_as_sf(frame_hum, coords = c("Long", "Lat"), crs = "EPSG:4326")

# We extract now the latitude raster
latitude <- st_as_sf(frame_hum, coords = c("Long", "Lat"), crs = "EPSG:4326") %>% dplyr::select(Latitude)

Raster <- raster(crs = "EPSG:4326", vals = 0, res = c(0.5, 0.5), ext = extent(c(-180, 180, -90, 90)))

rasterlatitude <- rasterize(latitude, Raster, field = 'Latitude', fun = mean, na.rm = T)

# We now go back to the monthly humidity

RasterJanhum <- rasterize(humidity[, 1], Raster, field = 'Jan', fun = mean, na.rm = T)
RasterFebhum <- rasterize(humidity[, 2], Raster, field = 'Feb', fun = mean, na.rm = T)
RasterMarhum <- rasterize(humidity[, 3], Raster, field = 'Mar', fun = mean, na.rm = T)
RasterAprhum <- rasterize(humidity[, 4], Raster, field = 'Apr', fun = mean, na.rm = T)
RasterMayhum <- rasterize(humidity[, 5], Raster, field = 'May', fun = mean, na.rm = T)
RasterJunhum <- rasterize(humidity[, 6], Raster, field = 'Jun', fun = mean, na.rm = T)
RasterJulhum <- rasterize(humidity[, 7], Raster, field = 'Jul', fun = mean, na.rm = T)
RasterAughum <- rasterize(humidity[, 8], Raster, field = 'Aug', fun = mean, na.rm = T)
RasterSephum <- rasterize(humidity[, 9], Raster, field = 'Sep', fun = mean, na.rm = T)
RasterOcthum <- rasterize(humidity[, 10], Raster, field = 'Oct', fun = mean, na.rm = T)
RasterNovhum <- rasterize(humidity[, 11], Raster, field = 'Nov', fun = mean, na.rm = T)
RasterDechum <- rasterize(humidity[, 12], Raster, field = 'Dec', fun = mean, na.rm = T)


mean_humidity <- (RasterJanhum + RasterFebhum + RasterMarhum + RasterAprhum + RasterMayhum + RasterJunhum + RasterJulhum + RasterAughum + RasterSephum + RasterOcthum + RasterNovhum + RasterDechum)/12

## wind

unzip_wind <- gzfile(wind,"rt")  
data_wind <- read.table(unzip_wind, header = F) 

# Wind is at 10m. However, we need to convert it to 2m. According to the litterature: U2 ~ 0.72*U10.
data_wind[, 3:14] <- data_wind[, 3:14]*0.72

colnames(data_wind) <- c("Lat", "Long", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
frame_wind <- data.frame(data_wind)


wind <- st_as_sf(frame_wind, coords = c("Long", "Lat"), crs = "EPSG:4326")

Raster <- raster(crs = "EPSG:4326", vals = 0, res = c(0.5, 0.5), ext = extent(c(-180, 180, -90, 90)))

RasterJanwind <- rasterize(wind[, 1], Raster, field = 'Jan', fun = mean, na.rm = T)
RasterFebwind <- rasterize(wind[, 2], Raster, field = 'Feb', fun = mean, na.rm = T)
RasterMarwind <- rasterize(wind[, 3], Raster, field = 'Mar', fun = mean, na.rm = T)
RasterAprwind <- rasterize(wind[, 4], Raster, field = 'Apr', fun = mean, na.rm = T)
RasterMaywind <- rasterize(wind[, 5], Raster, field = 'May', fun = mean, na.rm = T)
RasterJunwind <- rasterize(wind[, 6], Raster, field = 'Jun', fun = mean, na.rm = T)
RasterJulwind <- rasterize(wind[, 7], Raster, field = 'Jul', fun = mean, na.rm = T)
RasterAugwind <- rasterize(wind[, 8], Raster, field = 'Aug', fun = mean, na.rm = T)
RasterSepwind <- rasterize(wind[, 9], Raster, field = 'Sep', fun = mean, na.rm = T)
RasterOctwind <- rasterize(wind[, 10], Raster, field = 'Oct', fun = mean, na.rm = T)
RasterNovwind <- rasterize(wind[, 11], Raster, field = 'Nov', fun = mean, na.rm = T)
RasterDecwind <- rasterize(wind[, 12], Raster, field = 'Dec', fun = mean, na.rm = T)

average_wind <- (RasterJanwind + RasterFebwind + RasterMarwind + RasterAprwind + RasterMaywind + RasterJunwind + RasterJulwind + RasterAugwind + RasterSepwind + RasterOctwind + RasterNovwind + RasterDecwind)/12


## elevation 
unzip_elev <- gzfile(elev,"rt")  
data_elev <- read.table(unzip_elev, header = F) 

# Elevation is in km, let's convert it in meters
data_elev[, 3] <- data_elev[, 3]*1000

colnames(data_elev) <- c("Lat", "Long", "elev")
frame_elev <- data.frame(data_elev)

elevation <- st_as_sf(frame_elev, coords = c("Long", "Lat"), crs = "EPSG:4326")

Raster <- raster(crs = "EPSG:4326", vals = 0, res = c(0.5, 0.5), ext = extent(c(-180, 180, -90, 90)))

rasterelev <- rasterize(elevation, Raster, field = 'elev', fun = mean, na.rm = T)


CRU_list <- rep(list(0), 5)

CRU_list[[1]] <- mean_sunshine
CRU_list[[2]] <- mean_humidity
CRU_list[[3]] <- average_wind
CRU_list[[4]] <- rasterlatitude
CRU_list[[5]] <- rasterelev

CRU_list
}
