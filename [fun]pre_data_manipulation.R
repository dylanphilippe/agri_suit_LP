# We first create a list of 501 entries, one for each year ranging from 1500 to 2000.
# We will start by importing then precipitation for autumn and continue with the other seasons.
extent_pre <- c(-29.75, 39.75, 30.25, 70.75)

pre_data_manipulation <- function(aut, win, spr, sum){
  
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
}

# We have to remember that we still have the first line in the matrix correspond to the year with filled values (NA's)
# We will now merge the different seasons and create a list to have only yearly observations

precipitation <- rep(list(0), 501)

# We create a loop and remove the first row of each matrix and create a raster 
for (j in 1:501){
  precipitation[[j]] <- raster((mylist_aut[[j]][-1, ] + mylist_spr[[j]][-1, ] + mylist_sum[[j]][-1, ] + mylist_win[[j]][-1, ]), crs = CRSproject)
}

# We plug the extent
for (k in 1:501){
  extent(precipitation[[k]]) <- extent_pre
}

# We stack everything
precipitation_stack <- raster::stack(precipitation)
precipitation_stack 

}

