wd <- "SET WORKGING DIRECTORY HERE"
setwd(wd)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster, tmap, dplyr)
CRSproject <- "EPSG:4326"

extent_pre <- c(-30, 40, 30, 71)
  
  mylist_aut <- rep(list(0), 501)
  mylist_win <- rep(list(0), 501)
  mylist_spr <- rep(list(0), 501)
  mylist_sum <- rep(list(0), 501)
  
  aut <- as.matrix(read.table("inputs/Precipitation/prec-pauling-au.txt", sep = "", header = F, fill = T))
  win <- as.matrix(read.table("inputs/Precipitation/prec-pauling-wi.txt", sep = "", header = F, fill = T))
  spr <- as.matrix(read.table("inputs/Precipitation/prec-pauling-sp.txt", sep = "", header = F, fill = T))
  sum <- as.matrix(read.table("inputs/Precipitation/prec-pauling-su.txt", sep = "", header = F, fill = T))
  
  
  # Since we have 4 big matrices containing all the 501 years we have to split them
  # the first year 1500 is thus column 1:140 and row 1:83, hence we can create a loop using breaks a different interval. We have 41583 row in total.
  
  c1 <- seq(1, 41583, by = 83)
  c2 <- seq(83, 41583, by = 83)
  
  
  for (i in 1:501){
    mylist_aut[[i]] <- aut[c(c1[i]:c2[i]), ]
    mylist_win[[i]] <- win[c(c1[i]:c2[i]), ]
    mylist_spr[[i]] <- spr[c(c1[i]:c2[i]), ]
    mylist_sum[[i]] <- sum[c(c1[i]:c2[i]), ]
    
    # make rasters 
    mylist_aut[[i]] <- raster(mylist_aut[[i]], crs = CRSproject)
    extent(mylist_aut[[i]]) <- extent_pre
    mylist_win[[i]] <- raster(mylist_win[[i]], crs = CRSproject)
    extent(mylist_win[[i]]) <- extent_pre
    mylist_spr[[i]] <- raster(mylist_spr[[i]], crs = CRSproject)
    extent(mylist_spr[[i]]) <- extent_pre
    mylist_sum[[i]] <- raster(mylist_sum[[i]], crs = CRSproject)
    extent(mylist_sum[[i]]) <- extent_pre
  }

  
# Stack all the layers together
raster_winter_brick <- raster::brick(mylist_win)
raster_spring_brick <- raster::brick(mylist_spr)
raster_summer_brick <- raster::brick(mylist_sum)
raster_autumn_brick <- raster::brick(mylist_aut)

# Change names of raster layer
names_winter <- paste("winter", sep = "_", as.character(seq(1500, 2000, by = 1)))
names_spring <- paste("spring", sep = "_", as.character(seq(1500, 2000, by = 1)))
names_summer <- paste("summer", sep = "_", as.character(seq(1500, 2000, by = 1)))
names_autumn <- paste("autumn", sep = "_", as.character(seq(1500, 2000, by = 1)))

names(raster_winter_brick) <- names_winter
names(raster_spring_brick) <- names_spring
names(raster_summer_brick) <- names_summer
names(raster_autumn_brick) <- names_autumn

writeRaster(raster_winter_brick, filename = "./processed_data/precip_win.tif", bandorder = "BIL", overwrite = TRUE)   
writeRaster(raster_spring_brick, filename = "./processed_data/precip_spr.tif", bandorder = "BIL", overwrite = TRUE)   
writeRaster(raster_summer_brick, filename = "./processed_data/precip_sum.tif", bandorder = "BIL", overwrite = TRUE)   
writeRaster(raster_autumn_brick, filename = "./processed_data/precip_aut.tif", bandorder = "BIL", overwrite = TRUE)



