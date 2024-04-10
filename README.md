# A Time-Varying Index of Agricultural Suitability across Europe from 1500 - 2000

This repository is dedicated to the creation of the Agricultural Suitability Index.
The computation of the index has been entirely built using R version 4.3.0 (2023-04-21 ucrt) with the following device specifications: processor 11th Gen Intel(R) Core(TM) i7-1165G7 @ 2.80GHz 2.80 GHz, installed RAM 32.0 GB (31.7 GB usable) and system type 64-bit operating system, x64-based processor.

## Folder Structure and Description
- **Agricultural_Suitability.Rmd**  is the main replication file, setting the extent of the study, importing the raw data, processing the different parameters, constructing and saving the suitability index into a multi-layer raster: *suit.tif*. The file calls the following functions that have been compartmentalized for readability purposes. These scripts only define functions and do not need to be executed separately. 
  - [fun]CRU_manipulation.R is a function that processes the raw data from the Climate Research Unit (CRU) into a list before extraction.
  - [fun]temp_data_manipulation.R is a function that processes the raw temperature data and stores into a list: the mean, minimum, and maximum average temperature before extraction. 
  - [fun]pre_data_manipulation.R is a function that processes the raw precipitation data into a list before extraction.
  - [fun]evapo_grid.R is a function that computes the rate of reference evapotranspiration ($ET_0$) using the Penman-Monteith equation following the FAO GAEZ method.
- **Technical_validation.R** import, compare and save the various outputs from the technical validation part.
- **Figures_tables.R** generates all figures and tables shown in the manuscript. 
- **Pre_raster_generation.R** imports, processes, and saves the raw precipitation data into multi-layer rasters for the Winter, Spring, Summer and Autumn season: *precip_win.tif*, *precip_spr.tif*, *precip_sum.tif*, *precip_aut.tif*.
- **Temp_raster_generation.R** imports, processes, and saves the raw temperature data into multi-layer rasters for the Winter, Spring, Summer, and Autumn season: *temp_win.tif*, *temp_spr.tif*, *temp_sum.tif*, *temp_aut.tif*.

One way to load the *suit.tif* rasterstack from `figshare` is by simplying importing it with the `raster` package in R with `index <- raster::stack("suit.tif")`.
