
  
wd <- "C:/Users/DylanPhilippe/OneDrive - Wyss Academy for Nature/Documents/GitHub/Agricultural_Suitability"
setwd(wd)
extent_temp <- c(-25, 40, 35, 70)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster, tmap, dplyr)
  
# Let's first import the historical temperature dataset from Luterbacher. Here are some info on the README textfile:
# Grid: 0.5? x 0.5?  
# Spatial area: 25W - 40E und 30 - 70N
# Note: the date are valied for a 0.5x0.5deg box. The center of the box is always on a xx.25 coordinate	
# Time period: Winter 1500 - Autumn 2002 
# YearSeason followed by 9100 Gridpoints
# Seasons are given as  13(Winter, DJF), 14(Spring, MAM), 15 (Summer, JJA), 16 (Autumn, SON)

historical_temp <- as.matrix(read.table("./inputs/Temperature/TT_Europe_1500_2002.txt", sep = ""))
# The table is huge. We can see that the first column give the year and the season.
# Hence, the first line given by 150013 is the average temperature for winter 1500.
# We can henceforth create a loop over the lines to extract all the year and season and generate rasters out of it.

# dim(historical_temp)

# We have 2012 lines and 9101 columns
# We can first drop the first column as we know when it starts and when it ends

historical_temp <- historical_temp[, -1]
dim(historical_temp)

# We have now 9100 column which correspond to the number of gridpoints. Let's create the loop

# We now create the loop over each rows and store every matrix in a list
mydata <- rep(list(0), 2012)

for (i in 1:2012){
  mydata[[i]] <- matrix(historical_temp[i, ], nrow = 70, ncol = 130, byrow = T)
}

# We can now split the list and group it in 4 so that we have 503 lists with 4 raster inside: 1 for each season
# for each elements, [[1]] is winter, [[2]] is spring, [[3]] is summer and [[4]] is Autumn.
mydata <- split(mydata, rep(1:ceiling(2012/4), each = 4)[1:2012])

# We will now gather season together for the past 500 years creating 4 lists: Spring, Summer, Fall and Winter with according temperature from 1500 to 2002.
temp_winter <- rep(list(0), 503)
temp_spring <- rep(list(0), 503)
temp_summer <- rep(list(0), 503)
temp_autumn <- rep(list(0), 503)

for (i in 1:503){
  temp_winter[[i]] <- mydata[[i]][[1]]
  temp_spring[[i]] <- mydata[[i]][[2]]
  temp_summer[[i]] <- mydata[[i]][[3]]
  temp_autumn[[i]] <- mydata[[i]][[4]]
  
  # Replace -999.99 with NA's
  temp_winter[[i]][temp_winter[[i]] == -999.99] <- NA
  temp_spring[[i]][temp_spring[[i]] == -999.99] <- NA
  temp_summer[[i]][temp_summer[[i]] == -999.99] <- NA
  temp_autumn[[i]][temp_autumn[[i]] == -999.99] <- NA
  
  # Creat the rasters and put the extent
  temp_winter[[i]] <- raster(temp_winter[[i]], crs = CRSproject)
  extent(temp_winter[[i]]) <- extent_temp
  temp_spring[[i]] <- raster(temp_spring[[i]], crs = CRSproject)
  extent(temp_spring[[i]]) <- extent_temp
  temp_summer[[i]] <- raster(temp_summer[[i]], crs = CRSproject)
  extent(temp_summer[[i]]) <- extent_temp
  temp_autumn[[i]] <- raster(temp_autumn[[i]], crs = CRSproject)
  extent(temp_autumn[[i]]) <- extent_temp
}


# Stack all the layers together
raster_winter_brick <- raster::brick(temp_winter)
raster_spring_brick <- raster::brick(temp_spring)
raster_summer_brick <- raster::brick(temp_summer)
raster_autumn_brick <- raster::brick(temp_autumn)

# Change names of raster layer
names_winter <- paste("winter", sep = "_", as.character(seq(1500, 2002, by = 1)))
names_spring <- paste("spring", sep = "_", as.character(seq(1500, 2002, by = 1)))
names_summer <- paste("summer", sep = "_", as.character(seq(1500, 2002, by = 1)))
names_autumn <- paste("autumn", sep = "_", as.character(seq(1500, 2002, by = 1)))

names(raster_winter_brick) <- names_winter
names(raster_spring_brick) <- names_spring
names(raster_summer_brick) <- names_summer
names(raster_autumn_brick) <- names_autumn

writeRaster(raster_winter_brick, filename = "./processed_data/temp_win.tif", bandorder = "BIL", overwrite = TRUE)   
writeRaster(raster_spring_brick, filename = "./processed_data/temp_spr.tif", bandorder = "BIL", overwrite = TRUE)   
writeRaster(raster_summer_brick, filename = "./processed_data/temp_sum.tif", bandorder = "BIL", overwrite = TRUE)   
writeRaster(raster_autumn_brick, filename = "./processed_data/temp_aut.tif", bandorder = "BIL", overwrite = TRUE)   





