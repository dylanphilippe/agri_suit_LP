
temp_data_manipulation <- function(temp){

extent_temp <- c(-25, 40, 35, 70)
  
# Let's first import the historical temperature dataset from Luterbacher. Here are some info on the README textfile:
# Grid: 0.5? x 0.5?  
# Spatial area: 25W - 40E und 35S - 70N
# Note: the date are valied for a 0.5x0.5deg box. The center of the box is always on a xx.25 coordinate	
# Time period: Winter 1500 - Autumn 2002 
# YearSeason followed by 9100 Gridpoints
# Seasons are given as  13(Winter, DJF), 14(Spring, MAM), 15 (Summer, JJA), 16 (Autumn, SON)

historical_temp <- as.matrix(read.table(temp, sep = ""))
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
mydata <- split(mydata, rep(1:ceiling(2012/4), each = 4)[1:2012])

# We will now create a funcion that allow to compute mean, min and max temp for each year, and then store it into layers

REstack <- function(win, spr, sum, aut){
  temp <- win
  
  temp1 <- spr
  
  temp2 <- sum
  
  temp3 <- aut
  
  tempmin <- matrix(nrow = 70, ncol = 130, data = 0)
  
  
  for (i in 1:70)
    for (j in 1:130)
      if (temp[i, j] <= temp1[i, j] & temp[i, j] <= temp2[i, j] & temp[i, j] <= temp3[i, j]){
        tempmin[i, j] <- temp[i, j]
      } else if (temp1[i, j] <= temp[i, j] & temp1[i, j] <= temp2[i, j] & temp1[i, j] <= temp3[i, j]){
        tempmin[i, j] <- temp1[i, j]
      } else if (temp2[i, j] <= temp[i, j] & temp2[i, j] <= temp1[i, j] & temp2[i, j] <= temp3[i, j]){
        tempmin[i, j] <- temp2[i, j]
      } else if (temp3[i, j] <= temp[i, j] & temp3[i, j] <= temp1[i, j] & temp3[i, j] <= temp2[i, j]){
        tempmin[i, j] <- temp3[i, j]
      }
  
  tempmin_yyyy <- raster(tempmin, crs = CRSproject)
  tempmin_yyyy[tempmin_yyyy == -999.99] <- NA
  extent(tempmin_yyyy) <- extent_temp
  
  tempmax <- matrix(nrow = 70, ncol = 130, data = 0)
  
  
  for (i in 1:70)
    for (j in 1:130)
      if (temp[i, j] >= temp1[i, j] & temp[i, j] >= temp2[i, j] & temp[i, j] >= temp3[i, j]){
        tempmax[i, j] <- temp[i, j]
      } else if (temp1[i, j] >= temp[i, j] & temp1[i, j] >= temp2[i, j] & temp1[i, j] >= temp3[i, j]){
        tempmax[i, j] <- temp1[i, j]
      } else if (temp2[i, j] >= temp[i, j] & temp2[i, j] >= temp1[i, j] & temp2[i, j] >= temp3[i, j]){
        tempmax[i, j] <- temp2[i, j]
      } else if (temp3[i, j] >= temp[i, j] & temp3[i, j] >= temp1[i, j] & temp3[i, j] >= temp2[i, j]){
        tempmax[i, j] <- temp3[i, j]
      }
  
  tempmax_yyyy <- raster(tempmax, crs = CRSproject)
  tempmax_yyyy[tempmax_yyyy == -999.99] <- NA
  extent(tempmax_yyyy) <- extent_temp
  
  tempmean_yyyy <- (temp + temp1 + temp2 + temp3)/4
  tempmean_yyyy <- raster(tempmean_yyyy, crs = CRSproject)
  tempmean_yyyy[tempmean_yyyy == -999.99] <- NA
  extent(tempmean_yyyy) <- extent_temp
  
  TEMP_stack <- stack(tempmean_yyyy, tempmin_yyyy, tempmax_yyyy)
}

TEMP_mean <- rep(list(0), 503)
TEMP_min <- rep(list(0), 503)
TEMP_max <- rep(list(0), 503)

# We can now loop over the function to do that for each year and storing min, mean and max into the corresponding list
for (k in 1:503){
  TEMP_mean[[k]] <- REstack(mydata[[k]][[1]], mydata[[k]][[2]], mydata[[k]][[3]], mydata[[k]][[4]])$layer.1
  TEMP_min[[k]] <- REstack(mydata[[k]][[1]], mydata[[k]][[2]], mydata[[k]][[3]], mydata[[k]][[4]])$layer.2
  TEMP_max[[k]] <- REstack(mydata[[k]][[1]], mydata[[k]][[2]], mydata[[k]][[3]], mydata[[k]][[4]])$layer.3
}

# We can stack the list making 503 layers
temp_mean_1500_2002 <- raster::stack(TEMP_mean)
temp_min_1500_2002 <- raster::stack(TEMP_min)
temp_max_1500_2002 <- raster::stack(TEMP_max)

# we can store the 3 rasters into a list
temp_mean_min_max <- rep(list(0), 3)
temp_mean_min_max[[1]] <- temp_mean_1500_2002
temp_mean_min_max[[2]] <- temp_min_1500_2002
temp_mean_min_max[[3]] <- temp_max_1500_2002

temp_mean_min_max
}
