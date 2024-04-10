
################# Figure for Agri suit maps and time series ####################
rm(list = ls())
gc()

setwd("SET WORKING DIRECTORY HERE")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tmap, dplyr, zoo, ggplot2, sf, sandwich, latex2exp)

options(scipen = 999) # This option allows to remove the scientific notation

sf::sf_use_s2(FALSE)
load("./processed_data/grid_europe.Rdata")
load("./processed_data/grid_suit.Rdata")
load("./processed_data/europe.Rdata")


suit_1500 <- tm_shape(st_intersection(grid_suit[[170]], europe)) + tm_fill("suit", breaks = seq(0, 1, by = 0.2), palette = "RdYlGn", style = "cont", title = "Suitability index", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_suit[[170]], europe)) + tm_fill("suit", breaks = seq(0, 1, by = 0.1), palette = "RdYlGn", legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif") + tm_shape(europe) + tm_borders()
suit_1500

suit_1600 <- tm_shape(st_intersection(grid_suit[[196]], europe)) + tm_fill("suit", breaks = seq(0, 1, by = 0.2), palette = "RdYlGn", style = "cont", title = "Suitability index", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_suit[[196]], europe)) + tm_fill("suit", breaks = seq(0, 1, by = 0.1), palette = "RdYlGn", legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
suit_1600

# Bad year 1669
suit_1669 <- tm_shape(st_intersection(grid_suit[[221]], europe)) + tm_fill("suit", breaks = seq(0, 1, by = 0.2), palette = "RdYlGn", style = "cont", title = "Suitability index", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_suit[[221]], europe)) + tm_fill("suit", breaks = seq(0, 1, by = 0.1), palette = "RdYlGn", legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
suit_1669

suit_1700 <- tm_shape(st_intersection(grid_suit[[276]], europe)) + tm_fill("suit", breaks = seq(0, 1, by = 0.2), palette = "RdYlGn", style = "cont", title = "Suitability index", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_suit[[276]], europe)) + tm_fill("suit", breaks = seq(0, 1, by = 0.1), palette = "RdYlGn", legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif") + tm_shape(europe) + tm_borders()
suit_1700

suit_1800 <- tm_shape(st_intersection(grid_suit[[403]], europe)) + tm_fill("suit", breaks = seq(0, 1, by = 0.2), palette = "RdYlGn", style = "cont", title = "Suitability index", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_suit[[403]], europe)) + tm_fill("suit", breaks = seq(0, 1, by = 0.1), palette = "RdYlGn", legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
suit_1800

suit_1900 <- tm_shape(st_intersection(grid_suit[[490]], europe)) + tm_fill("suit", breaks = seq(0, 1, by = 0.2), palette = "RdYlGn", style = "cont", title = "Suitability index", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_suit[[490]], europe)) + tm_fill("suit", breaks = seq(0, 1, by = 0.1), palette = "RdYlGn", legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
suit_1900


tmap_save(suit_1500, filename = "./figures/suit_1669.png")
tmap_save(suit_1600, filename = "./figures/suit_1695.png")
tmap_save(suit_1669, filename = "./figures/suit_1720.png")
tmap_save(suit_1700, filename = "./figures/suit_1775.png")
tmap_save(suit_1800, filename = "./figures/suit_1902.png")
tmap_save(suit_1900, filename = "./figures/suit_1989.png") 

# Interesting years standard devations

# 1669 lowest pre europe 
# 1695 lowest temperatures
# 1720 pre highest europe 
# Highest crop suit 1775
# Lowest crop suit 1902
# 1989 highest temperature 

grid_europe_gid <- grid_europe
grid_europe_gid <- left_join(grid_europe_gid %>% st_drop_geometry(), grid_suit[[1]] %>% st_drop_geometry() %>% dplyr::select(c("gid", "suit")), by = "gid")
years <- as.character(seq(1500, 2000, by = 1))

for (i in 2:501){
  grid_europe_gid <- left_join(grid_europe_gid, grid_suit[[i]] %>% st_drop_geometry() %>% dplyr::select("gid", "suit"), by = "gid")
}

grid_europe$mean <- rowMeans(grid_europe_gid[, 2:502], na.rm = T)
grid_europe$sd <- apply(grid_europe_gid[, 2:502], 1, sd, na.rm = T) 


suit_1669 <- st_intersection(grid_suit[[170]], europe)
suit_1669 <- left_join(suit_1669, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
suit_1669 <- suit_1669 %>% mutate(suit_1669_z = (suit - mean)/sd)

suit_1669_shape <- tm_shape(suit_1669) + tm_fill("suit_1669_z", palette = "RdYlGn", title = "Z-score", breaks = seq(-22, 3.5, by = 5), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(suit_1669) + tm_fill("suit_1669_z", palette = "RdYlGn", title = "Pre (sd)", breaks = seq(-22, 3.5, by = 2.55), legend.show = F, showNA = F, colorNA = NULL)  +
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif") + tm_shape(europe) + tm_borders()
suit_1669_shape


suit_1695 <- st_intersection(grid_suit[[196]], europe)
suit_1695 <- left_join(suit_1695, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
suit_1695 <- suit_1695 %>% mutate(suit_1695_z = (suit - mean)/sd)

suit_1695_shape <- tm_shape(suit_1695) + tm_fill("suit_1695_z", palette = "RdYlGn", title = "sd Suit 1695 (lowest temp)", breaks = seq(-6.2, 1.2, by = 1.48), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(suit_1695) + tm_fill("suit_1695_z", palette = "RdYlGn", title = "Pre (sd)", breaks = seq(-6.2, 1.2, by = 0.74), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
suit_1695_shape

suit_1720 <- st_intersection(grid_suit[[221]], europe)
suit_1720 <- left_join(suit_1720, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
suit_1720 <- suit_1720 %>% mutate(suit_1720_z = (suit - mean)/sd)

suit_1720_shape <- tm_shape(suit_1720) + tm_fill("suit_1720_z", palette = "RdYlGn", title = "sd Suit 1720 (highest pre)", breaks = seq(-3, 2, by = 1), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(suit_1720) + tm_fill("suit_1720_z", palette = "RdYlGn", title = "Pre (sd)", breaks = seq(-3, 2, by = 0.5), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
suit_1720_shape

suit_1775 <- st_intersection(grid_suit[[276]], europe)
suit_1775 <- left_join(suit_1775, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
suit_1775 <- suit_1775 %>% mutate(suit_1775_z = (suit - mean)/sd)

suit_1775_shape <- tm_shape(suit_1775) + tm_fill("suit_1775_z", palette = "RdYlGn", title = "Z-score", breaks = seq(-1.1, 4.5, by = 1.12), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(suit_1775) + tm_fill("suit_1775_z", palette = "RdYlGn", title = "Pre (sd)", breaks = seq(-1.2, 5, by = 0.56), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif") + tm_shape(europe) + tm_borders()
suit_1775_shape

suit_1902 <- st_intersection(grid_suit[[403]], europe)
suit_1902 <- left_join(suit_1902, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
suit_1902 <- suit_1902 %>% mutate(suit_1902_z = (suit - mean)/sd)

suit_1902_shape <- tm_shape(suit_1902) + tm_fill("suit_1902_z", palette = "RdYlGn", title = "sd Suit 1902 (lowest suit)", breaks = seq(-15, 2.4, by = 3.48), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(suit_1902) + tm_fill("suit_1902_z", palette = "RdYlGn", title = "Pre (sd)", breaks = seq(-15, 2.4, by = 1.74), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
suit_1902_shape

suit_1989 <- st_intersection(grid_suit[[490]], europe)
suit_1989 <- left_join(suit_1989, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
suit_1989 <- suit_1989 %>% mutate(suit_1989_z = (suit - mean)/sd)

suit_1989_shape <- tm_shape(suit_1989) + tm_fill("suit_1989_z", palette = "RdYlGn", title = "sd Suit 1989 (high temp)", breaks = seq(-14.5, 4.5, by = 3.8), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(suit_1989) + tm_fill("suit_1989_z", palette = "RdYlGn", title = "Pre (sd)", breaks = seq(-14.5, 4.5, by = 1.9), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
suit_1989_shape


tmap_save(suit_1669_shape, filename = "./figures/suit_1669_z.png")
tmap_save(suit_1695_shape, filename = "./figures/suit_1695_z.png")
tmap_save(suit_1720_shape, filename = "./figures/suit_1720_z.png")
tmap_save(suit_1775_shape, filename = "./figures/suit_1775_z.png")
tmap_save(suit_1902_shape, filename = "./figures/suit_1902_z.png")
tmap_save(suit_1989_shape, filename = "./figures/suit_1989_z.png")


# Time series
grid_agrisuit <- grid_europe

# We create a mean agricultural suitability per year 
for (i in 1:501){
  # 1 indictating that we are in europe
  grid_suit[[i]]$eur <- 1
  
  # We take the average over all Europe
  grid_suit[[i]] <- grid_suit[[i]] %>% group_by(eur) %>% mutate(avg_suit = mean(suit, na.rm = T)) %>% ungroup()
}

time_series_suit <- grid_suit[[1]] %>% st_drop_geometry() %>% dplyr::select(avg_suit)

for (i in 2:501){
  time_series_suit <- cbind(time_series_suit, grid_suit[[i]] %>% st_drop_geometry() %>% dplyr::select(avg_suit))  
}

time_series_suit <- time_series_suit[1, ]
time_series_suit <- t(time_series_suit) 
time_series_suit <- time_series_suit %>% data.frame()
time_series_suit <- time_series_suit %>% rename("agri_suit" = X1)
rownames(time_series_suit) <- NULL
year <- seq(1500, 2000, by = 1)

time_series_suit$year <- year

time_series_suit <- time_series_suit %>%
  mutate(mov_avg_25 = rollapply(agri_suit, width = 25, 
                                FUN = mean, na.rm = TRUE, fill = NA, align = "right"))

ggplot(time_series_suit, x = year) + 
  geom_line(aes(y = agri_suit, x = year, color = "Land Suitability"), show.legend = F)  +
  theme_minimal(base_family = "serif", base_size = 18) + 
  theme(axis.line.x = element_line(size = 1),
        axis.ticks.x = element_line(size = 1)) +
  labs(x = "Years", y = "Suitability index (Suit)", color = "Land Suitability") +
  scale_color_manual(values = c("Land Suitability" = "black"))
ggsave(filename = "./figures/agri_suit.png", bg = "white", device = "png", width=12, height=6)

# 25 years moving average
ggplot(time_series_suit, x = year) + 
  geom_line(aes(y = mov_avg_25, x = year, color = "Land Suitability"), show.legend = F) +
  theme_minimal(base_family = "serif", base_size = 18) + 
  theme(axis.line.x = element_line(size = 1),
        axis.ticks.x = element_line(size = 1)) +
  labs(x = "Years", y = "Suitability index (Suit)", color = "Land Suitability") +
  scale_color_manual(values = c("Land Suitability" = "black")) 
ggsave(filename = "./figures/agri_suit_25ma.png", bg = "white", device = "png", width=12, height=6)








############################ Figure precipitation #############################
# Figure for precipitation maps and time series
rm(list = ls())
gc()

load("./processed_data/grid_europe.Rdata")
load("./processed_data/grid_pre.Rdata")
load("./processed_data/europe.Rdata")

pre_1669 <- tm_shape(st_intersection(grid_pre[[170]], europe)) + tm_fill("pre_1669", palette = "Blues", breaks = seq(0, 3500, by = 500), style = "cont", title = "Precipitation (mm/year)", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_pre[[170]], europe)) + tm_fill("pre_1669", palette = "Blues", breaks = seq(0, 3500, by = 250), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif") + tm_shape(europe) + tm_borders()
pre_1669


pre_1695 <- tm_shape(st_intersection(grid_pre[[196]], europe)) + tm_fill("pre_1695", palette = "Blues", breaks = seq(0, 3500, by = 500), style = "cont", title = "Precipitation (mm/year)", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_pre[[196]], europe)) + tm_fill("pre_1695", palette = "Blues", breaks = seq(0, 3500, by = 250),legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
pre_1695

# Bad year suitability: 1669
pre_1720 <- tm_shape(st_intersection(grid_pre[[221]], europe)) + tm_fill("pre_1720", palette = "Blues", breaks = seq(0, 3500, by = 500), style = "cont", title = "Precipitation (mm/year)", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_pre[[221]], europe)) + tm_fill("pre_1720", palette = "Blues", breaks = seq(0, 3500, by = 250), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
pre_1720

pre_1775 <- tm_shape(st_intersection(grid_pre[[276]], europe)) + tm_fill("pre_1775", palette = "Blues", breaks = seq(0, 3500, by = 500), style = "cont", title = "Precipitation (mm/year)", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_pre[[276]], europe)) + tm_fill("pre_1775", palette = "Blues", breaks = seq(0, 3500, by = 250), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif") + tm_shape(europe) + tm_borders()
pre_1775

pre_1902 <- tm_shape(st_intersection(grid_pre[[403]], europe)) + tm_fill("pre_1902", palette = "Blues", breaks = seq(0, 3500, by = 500), style = "cont", title = "Precipitation (mm/year)", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_pre[[403]], europe)) + tm_fill("pre_1902", palette = "Blues", breaks = seq(0, 3500, by = 250), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
pre_1902

pre_1989 <- tm_shape(st_intersection(grid_pre[[490]], europe)) + tm_fill("pre_1989", palette = "Blues", breaks = seq(0, 3500, by = 500), style = "cont", title = "Precipitation (mm/year)", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_pre[[490]], europe)) + tm_fill("pre_1989", palette = "Blues", breaks = seq(0, 3500, by = 250), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
pre_1989


tmap_save(pre_1669, filename = "./figures/pre_1669.png")
tmap_save(pre_1695, filename = "./figures/pre_1695.png")
tmap_save(pre_1720, filename = "./figures/pre_1720.png")
tmap_save(pre_1775, filename = "./figures/pre_1775.png")
tmap_save(pre_1902, filename = "./figures/pre_1902.png")
tmap_save(pre_1989, filename = "./figures/pre_1989.png")


### The standardized version 
# Creat mean and sd for each grid over the past 500 years

# We can loop and left join the temperature and then apply rownmeans and sd for row
grid_europe_gid <- grid_europe
grid_europe_gid <- left_join(grid_europe_gid %>% st_drop_geometry(), grid_pre[[1]] %>% st_drop_geometry() %>% dplyr::select(c("gid", "pre_1500")), by = "gid")
years <- as.character(seq(1500, 2000, by = 1))

for (i in 2:501){
  grid_europe_gid <- left_join(grid_europe_gid, grid_pre[[i]] %>% st_drop_geometry() %>% dplyr::select("gid", paste("pre", years[i], sep = "_")), by = "gid")
}

grid_europe$mean <- rowMeans(grid_europe_gid[, 2:502], na.rm = T)
grid_europe$sd <- apply(grid_europe_gid[, 2:502], 1, sd, na.rm = T) 


preSD_1669 <- st_intersection(grid_pre[[170]], europe)
preSD_1669 <- left_join(preSD_1669, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
preSD_1669 <- preSD_1669 %>% mutate(preSD_1669_z = (pre_1669 - mean)/sd)

preSD_1669_shape <- tm_shape(preSD_1669) + tm_fill("preSD_1669_z", palette = "RdYlBu", title = "Z-score", breaks = seq(-5, 3.6, by = 1.72), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(preSD_1669) + tm_fill("preSD_1669_z", palette = "RdYlBu", title = "Pre (sd)", breaks = seq(-5, 3.6, by = 0.86), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif") + tm_shape(europe) + tm_borders()
preSD_1669_shape

preSD_1695 <- st_intersection(grid_pre[[196]], europe)
preSD_1695 <- left_join(preSD_1695, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
preSD_1695 <- preSD_1695 %>% mutate(preSD_1695_z = (pre_1695 - mean)/sd)

preSD_1695_shape <- tm_shape(preSD_1695) + tm_fill("preSD_1695_z", palette = "RdYlGn", title = "preSD 1695 (lowest temp)", breaks = seq(-1.7, 1.6, by = 0.66), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(preSD_1695) + tm_fill("preSD_1695_z", palette = "RdYlGn", title = "Pre (sd)", breaks = seq(-1.7, 1.6, by = 0.33), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
preSD_1695_shape

preSD_1720 <- st_intersection(grid_pre[[221]], europe)
preSD_1720 <- left_join(preSD_1720, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
preSD_1720 <- preSD_1720 %>% mutate(preSD_1720_z = (pre_1720 - mean)/sd)

preSD_1720_shape <- tm_shape(preSD_1720) + tm_fill("preSD_1720_z", palette = "RdYlGn", title = "preSD 1720 (highest pre)", breaks = seq(-3, 4, by = 1.4), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(preSD_1720) + tm_fill("preSD_1720_z", palette = "RdYlGn", title = "Pre (sd)", breaks = seq(-3, 4, by = 0.7), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
preSD_1720_shape

preSD_1775 <- st_intersection(grid_pre[[276]], europe)
preSD_1775 <- left_join(preSD_1775, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
preSD_1775 <- preSD_1775 %>% mutate(preSD_1775_z = (pre_1775 - mean)/sd)

preSD_1775_shape <- tm_shape(preSD_1775) + tm_fill("preSD_1775_z", palette = "RdYlBu", title = "Z-score", breaks = seq(-1.5, 1.6, by = 0.62), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(preSD_1775) + tm_fill("preSD_1775_z", palette = "RdYlBu", title = "Pre (sd)", breaks = seq(-1.5, 1.6, by = 0.31), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif") + tm_shape(europe) + tm_borders()
preSD_1775_shape

preSD_1902 <- st_intersection(grid_pre[[403]], europe)
preSD_1902 <- left_join(preSD_1902, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
preSD_1902 <- preSD_1902 %>% mutate(preSD_1902_z = (pre_1902 - mean)/sd)

preSD_1902_shape <- tm_shape(preSD_1902) + tm_fill("preSD_1902_z", palette = "RdYlGn", title = "preSD 1902 (lowest suit)", breaks = seq(-4.5, 4, by = 1.7), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(preSD_1902) + tm_fill("preSD_1902_z", palette = "RdYlGn", title = "Pre (sd)", breaks = seq(-4.5, 4, by = 0.85), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
preSD_1902_shape

preSD_1989 <- st_intersection(grid_pre[[490]], europe)
preSD_1989 <- left_join(preSD_1989, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
preSD_1989 <- preSD_1989 %>% mutate(preSD_1989_z = (pre_1989 - mean)/sd)

preSD_1989_shape <- tm_shape(preSD_1989) + tm_fill("preSD_1989_z", palette = "RdYlGn", title = "preSD 1989 (high temp)", breaks = seq(-4.2, 4.5, by = 1.74), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(preSD_1989) + tm_fill("preSD_1989_z", palette = "RdYlGn", title = "Pre (sd)", breaks = seq(-4.2, 4.5, by = 0.87), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
preSD_1989_shape


tmap_save(preSD_1669_shape, filename = "./figures/preSD_1669.png")
tmap_save(preSD_1695_shape, filename = "./figures/preSD_1695.png")
tmap_save(preSD_1720_shape, filename = "./figures/preSD_1720.png")
tmap_save(preSD_1775_shape, filename = "./figures/preSD_1775.png")
tmap_save(preSD_1902_shape, filename = "./figures/preSD_1902.png")
tmap_save(preSD_1989_shape, filename = "./figures/preSD_1989.png")


# The time series 
grid_mean_pre_time_s <- grid_pre

years <- seq(1500, 2000, by = 1)

# We create a mean agricultural suitability per year 
for (i in 1:501){
  # 1 indictating that we are in europe
  grid_mean_pre_time_s[[i]]$eur <- 1
  grid_mean_pre_time_s[[i]] <- grid_mean_pre_time_s[[i]] %>% st_drop_geometry() 
  colnames(grid_mean_pre_time_s[[i]]) <- c("gid", "precip", "eur")
  
  # We take the average over all Europe
  grid_mean_pre_time_s[[i]] <- grid_mean_pre_time_s[[i]] %>% group_by(eur) %>% mutate(avg_pre = mean(precip, na.rm = T)) %>% ungroup()
}


time_series_pre <- grid_mean_pre_time_s[[1]] %>% st_drop_geometry() %>% dplyr::select(avg_pre)

for (i in 2:501){
  time_series_pre <- cbind(time_series_pre, grid_mean_pre_time_s[[i]] %>% st_drop_geometry() %>% dplyr::select(avg_pre))  
}

time_series_pre <- time_series_pre[1, ]
time_series_pre <- t(time_series_pre) 
time_series_pre <- time_series_pre %>% data.frame()
time_series_pre <- time_series_pre %>% rename("pre" = X1)
rownames(time_series_pre) <- NULL
year <- seq(1500, 2000, by = 1)

time_series_pre$year <- year

time_series_pre <- time_series_pre %>%
  mutate(mov_avg_25 = rollapply(pre, width = 25, 
                                FUN = mean, na.rm = TRUE, fill = NA, align = "right"))



ggplot(time_series_pre, x = year) + 
  geom_line(aes(y = pre, x = year, color = "Yearly mean precipitation"), show.legend = F) +
  theme_minimal(base_family = "serif", base_size = 18) + 
  theme(axis.line.x = element_line(size = 1),
        axis.ticks.x = element_line(size = 1)) +
  labs(x = "Years", y = "Mean precipitation (mm)", color = "Yearly mean precipitation") +
  scale_color_manual(values = c("Yearly mean precipitation" = "black"))
ggsave(filename = "./figures/pre_time_s.png", bg = "white", width=12, height=6)

# 25 years moving average
ggplot(time_series_pre, x = year) + 
  geom_line(aes(y = mov_avg_25, x = year, color = "Yearly mean precipitation"), show.legend = F) +
  theme_minimal(base_family = "serif", base_size = 18) + 
  theme(axis.line.x = element_line(size = 1),
        axis.ticks.x = element_line(size = 1)) +
  labs(x = "Years", y = "Mean precipitation (mm)", color = "Yearly mean precipitation") +
  scale_color_manual(values = c("Yearly mean precipitation" = "black")) 
ggsave(filename = "./figures/pre_time_s_25.png", bg = "white", width=12, height=6)









########################## Figures Temperature ##################################
# Figure for temperature maps and time series
rm(list = ls())
gc()

load("./processed_data/grid_mean_temp.Rdata")
load("./processed_data/grid_europe.Rdata")
load("./processed_data/europe.Rdata")

temp_1669 <- tm_shape(st_intersection(grid_mean_temp[[170]], europe)) + tm_fill("tmp_1669", breaks = seq(-10, 25, by = 5), palette = "-RdYlBu", title = "Temperature (°C)", legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_mean_temp[[170]], europe)) + tm_fill("tmp_1669", breaks = seq(-10, 25, by = 2), palette = "-RdYlBu", title = "Mean temp (°C)", legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif") + tm_shape(europe) + tm_borders()
temp_1669

temp_1695 <- tm_shape(st_intersection(grid_mean_temp[[196]], europe)) + tm_fill("tmp_1695", breaks = seq(-10, 25, by = 5), palette = "-RdYlBu", title = "Temperature (°C)", legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_mean_temp[[196]], europe)) + tm_fill("tmp_1695", breaks = seq(-10, 25, by = 2), palette = "-RdYlBu", title = "Mean temp (°C)", legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
temp_1695


temp_1720 <- tm_shape(st_intersection(grid_mean_temp[[221]], europe)) + tm_fill("tmp_1720", breaks = seq(-10, 25, by = 5), palette = "-RdYlBu", title = "Temperature (°C)", legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_mean_temp[[221]], europe)) + tm_fill("tmp_1720", breaks = seq(-10, 25, by = 2), palette = "-RdYlBu", title = "Mean temp (°C)", legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
temp_1720


temp_1775 <- tm_shape(st_intersection(grid_mean_temp[[276]], europe)) + tm_fill("tmp_1775", breaks = seq(-10, 25, by = 5), palette = "-RdYlBu", title = "Temperature (°C)", legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_mean_temp[[276]], europe)) + tm_fill("tmp_1775", breaks = seq(-10, 25, by = 2), palette = "-RdYlBu", title = "Mean temp (°C)", legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif") + tm_shape(europe) + tm_borders()
temp_1775

temp_1902 <- tm_shape(st_intersection(grid_mean_temp[[403]], europe)) + tm_fill("tmp_1902", breaks = seq(-10, 25, by = 5), palette = "-RdYlBu", title = "Temperature (°C)", legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_mean_temp[[403]], europe)) + tm_fill("tmp_1902", breaks = seq(-10, 25, by = 2), palette = "-RdYlBu", title = "Mean temp (°C)", legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
temp_1902

temp_1989 <- tm_shape(st_intersection(grid_mean_temp[[490]], europe)) + tm_fill("tmp_1989", breaks = seq(-10, 25, by = 5), palette = "-RdYlBu", title = "Temperature (°C)", legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(st_intersection(grid_mean_temp[[490]], europe)) + tm_fill("tmp_1989", breaks = seq(-10, 25, by = 2), palette = "-RdYlBu", title = "Mean temp (°C)", legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
temp_1989

tmap_save(temp_1669, filename = "./figures/temp_1669.png")
tmap_save(temp_1695, filename = "./figures/temp_1695.png")
tmap_save(temp_1720, filename = "./figures/temp_1720.png")
tmap_save(temp_1775, filename = "./figures/temp_1775.png")
tmap_save(temp_1902, filename = "./figures/temp_1902.png")
tmap_save(temp_1989, filename = "./figures/temp_1989.png")


### The standardized version 
# Creat mean and sd for each grid over the past 500 years

# We can loop and left join the temperature and then apply rownmeans and sd for row

grid_europe_gid <- grid_europe
grid_europe_gid <- left_join(grid_europe_gid %>% st_drop_geometry(), grid_mean_temp[[1]] %>% st_drop_geometry() %>% dplyr::select(c("gid", "tmp_1500")), by = "gid")
years <- as.character(seq(1500, 2000, by = 1))

for (i in 2:501){
  grid_europe_gid <- left_join(grid_europe_gid, grid_mean_temp[[i]] %>% st_drop_geometry() %>% dplyr::select("gid", paste("tmp", years[i], sep = "_")), by = "gid")
}

grid_europe$mean <- rowMeans(grid_europe_gid[, 2:502], na.rm = T)
grid_europe$sd <- apply(grid_europe_gid[, 2:502], 1, sd, na.rm = T) 


tempSD_1669 <- st_intersection(grid_mean_temp[[170]], europe)
tempSD_1669 <- left_join(tempSD_1669, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
tempSD_1669 <- tempSD_1669 %>% mutate(tempSD_1669_z = (tmp_1669 - mean)/sd)

tempSD_1669_shape <- tm_shape(tempSD_1669) + tm_fill("tempSD_1669_z", palette = "-RdYlBu", title = "Z-score", breaks = seq(-0.4, 1.2, by = 0.32), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(tempSD_1669) + tm_fill("tempSD_1669_z", palette = "-RdYlBu", title = "Pre (sd)", breaks = seq(-0.4, 1.2, by = 0.16), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif") + tm_shape(europe) + tm_borders()
tempSD_1669_shape

tempSD_1695 <- st_intersection(grid_mean_temp[[196]], europe)
tempSD_1695 <- left_join(tempSD_1695, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
tempSD_1695 <- tempSD_1695 %>% mutate(tempSD_1695_z = (tmp_1695 - mean)/sd)

tempSD_1695_shape <- tm_shape(tempSD_1695) + tm_fill("tempSD_1695_z", palette = "RdYlGn", title = "tempSD 1695 (lowest temp)", breaks = seq(-4.3, 1.6, by = 1.18), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(tempSD_1695) + tm_fill("tempSD_1695_z", palette = "RdYlGn", title = "Pre (sd)", breaks = seq(-4.3, 1.6, by = 0.59), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
tempSD_1695_shape

tempSD_1720 <- st_intersection(grid_mean_temp[[221]], europe)
tempSD_1720 <- left_join(tempSD_1720, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
tempSD_1720 <- tempSD_1720 %>% mutate(tempSD_1720_z = (tmp_1720 - mean)/sd)

tempSD_1720_shape <- tm_shape(tempSD_1720) + tm_fill("tempSD_1720_z", palette = "RdYlGn", title = "tempSD 1720 (highest pre)", breaks = seq(-1.2, 0.08, by = 0.256), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(tempSD_1720) + tm_fill("tempSD_1720_z", palette = "RdYlGn", title = "Pre (sd)", breaks = seq(-1.2, 0.08, by = 0.128), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
tempSD_1720_shape

tempSD_1775 <- st_intersection(grid_mean_temp[[276]], europe)
tempSD_1775 <- left_join(tempSD_1775, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
tempSD_1775 <- tempSD_1775 %>% mutate(tempSD_1775_z = (tmp_1775 - mean)/sd)

tempSD_1775_shape <- tm_shape(tempSD_1775) + tm_fill("tempSD_1775_z", palette = "-RdYlBu", title = "Z-score", breaks = seq(-0.6, 1.8, by = 0.48), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(tempSD_1775) + tm_fill("tempSD_1775_z", palette = "-RdYlBu", title = "Pre (sd)", breaks = seq(-0.6, 1.8, by = 0.24), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif") + tm_shape(europe) + tm_borders()
tempSD_1775_shape

tempSD_1902 <- st_intersection(grid_mean_temp[[403]], europe)
tempSD_1902 <- left_join(tempSD_1902, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
tempSD_1902 <- tempSD_1902 %>% mutate(tempSD_1902_z = (tmp_1902 - mean)/sd)

tempSD_1902_shape <- tm_shape(tempSD_1902) + tm_fill("tempSD_1902_z", palette = "RdYlGn", title = "tempSD 1902 (lowest suit)", breaks = seq(-4.4, 2, by = 1.28), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(tempSD_1902) + tm_fill("tempSD_1902_z", palette = "RdYlGn", title = "Pre (sd)", breaks = seq(-4.4, 2, by = 0.64), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
tempSD_1902_shape

tempSD_1989 <- st_intersection(grid_mean_temp[[490]], europe)
tempSD_1989 <- left_join(tempSD_1989, grid_europe %>% st_drop_geometry() %>% dplyr::select(gid, mean, sd), by = "gid")
tempSD_1989 <- tempSD_1989 %>% mutate(tempSD_1989_z = (tmp_1989 - mean)/sd)

tempSD_1989_shape <- tm_shape(tempSD_1989) + tm_fill("tempSD_1989_z", palette = "RdYlGn", title = "tempSD 1989 (high temp)", breaks = seq(-2.45, 3.7, by = 1.23), legend.show = T, style = "cont", showNA = F, colorNA = NULL) +
  tm_shape(tempSD_1989) + tm_fill("tempSD_1989_z", palette = "RdYlGn", title = "Pre (sd)", breaks = seq(-2.45, 3.7, by = 0.615), legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif")
tempSD_1989_shape


tmap_save(tempSD_1669_shape, filename = "./figures/tempSD_1669.png")
tmap_save(tempSD_1695_shape, filename = "./figures/tempSD_1695.png")
tmap_save(tempSD_1720_shape, filename = "./figures/tempSD_1720.png")
tmap_save(tempSD_1775_shape, filename = "./figures/tempSD_1775.png")
tmap_save(tempSD_1902_shape, filename = "./figures/tempSD_1902.png")
tmap_save(tempSD_1989_shape, filename = "./figures/tempSD_1989.png")


# The time series 
grid_temp <- grid_europe
years <- seq(1500, 2000, by = 1)

grid_mean_temp_time_s <- grid_mean_temp
# We create a mean agricultural suitability per year 
for (i in 1:501){
  # 1 indictating that we are in europe
  grid_mean_temp_time_s[[i]]$eur <- 1
  grid_mean_temp_time_s[[i]] <- grid_mean_temp_time_s[[i]] %>% st_drop_geometry()
  colnames(grid_mean_temp_time_s[[i]]) <- c("gid", "tmp", "eur")
  
  # We take the average over all Europe
  grid_mean_temp_time_s[[i]] <- grid_mean_temp_time_s[[i]] %>% group_by(eur) %>% mutate(avg_temp = mean(tmp, na.rm = T)) %>% ungroup()
}


time_series_temp <- grid_mean_temp_time_s[[1]] %>% st_drop_geometry() %>% dplyr::select(avg_temp)

for (i in 2:501){
  time_series_temp <- cbind(time_series_temp, grid_mean_temp_time_s[[i]] %>% st_drop_geometry() %>% dplyr::select(avg_temp))  
}

time_series_temp <- time_series_temp[1, ]
time_series_temp <- t(time_series_temp) 
time_series_temp <- time_series_temp %>% data.frame()
time_series_temp <- time_series_temp %>% rename("temp" = X1)
rownames(time_series_temp) <- NULL
year <- seq(1500, 2000, by = 1)

time_series_temp$year <- year

time_series_temp <- time_series_temp %>%
  mutate(mov_avg_25 = rollapply(temp, width = 25, 
                                FUN = mean, na.rm = TRUE, fill = NA, align = "right"))

ggplot(time_series_temp, x = year) + 
  geom_line(aes(y = temp, x = year, color = "Yearly mean temperature"), show.legend = F) +
  theme_minimal(base_family = "serif", base_size = 18) + 
  theme(axis.line.x = element_line(size = 1),
        axis.ticks.x = element_line(size = 1)) +
  labs(x = "Years", y = "Mean temperature (°C)", color = "Yearly mean temperature") +
  scale_y_continuous(limits = c(5, 9), breaks = seq(5, 9, by = 1)) +
  scale_color_manual(values = c("Yearly mean temperature" = "black"))
ggsave(filename = "./figures/temp_time_s.png", units = "in", bg = "white", width=12, height=6)

# 25 years moving average
ggplot(time_series_temp, x = year) + 
  geom_line(aes(y = mov_avg_25, x = year, color = "Yearly mean temperature"), show.legend = F) +
  theme_minimal(base_family = "serif", base_size = 18) + 
  theme(axis.line.x = element_line(size = 1),
        axis.ticks.x = element_line(size = 1)) +
  labs(x = "Years", y = "Mean temperature (°C)", color = "Yearly mean temperature") +
  scale_y_continuous(limits = c(6.75, 7.75), breaks = seq(6.75, 7.75, by = 0.5)) +
  scale_color_manual(values = c("Yearly mean temperature" = "black")) 
ggsave(filename = "./figures/temp_time_s_25.png", bg = "white", width=12, height=6)









########################### Figure/plots for Technical validation ################

rm(list = ls())
gc()

# Load the data 
load("./processed_data/tech_val.Rdata")

ols1 <- lm(raman_suit ~ suit_30_raman, data = tech_val)
coef_raman <- coef(ols1)
ols1

# Suitatbility ramankutty 
ggplot(data = tech_val, aes(x = suit_30_raman, y = raman_suit)) + geom_point(alpha = 0.3) +
  labs(y = "Suitability index (Ramankutty et al. 2002)", x = "Suitability index (Suit)") +
  theme_minimal(base_family = "serif", base_size = 18) +
  annotate("segment", x = 0, xend = 1, y = 0, yend = 1, color = "red3", size = 1, lty = "dashed") +
  geom_smooth(method = "lm", col = "red3", se = F, lty = "solid")+ 
  annotate("text", x = 0.02, y = 0.99, label = paste("y = ", round(coef_raman[2], digits = 2), "x"), 
           hjust = 0, vjust = 1, color = "black", size = 4) + 
  annotate("text", x = 0.005, y = 0.95, label = TeX("R$^{2}$"), 
           hjust = 0, vjust = 1, color = "black", size = 4)  + 
  annotate("text", x = 0.035, y = 0.94, label = paste("=", round(summary(ols1)$r.squared, digits = 2)), 
           hjust = 0, vjust = 1, color = "black", size = 4) 
ggsave(filename = "./figures/TECH_raman_suit.png", bg = "white", height = 6, width = 9, dpi = 300)


# FAO suitability
ols2 <- lm(FAO_suit ~ suit_30_FAO, data = tech_val)
coef_FAO <- coef(ols2)

ggplot(data = tech_val, aes(x = suit_30_FAO, y = FAO_suit)) + geom_point(alpha = 0.3) +
  labs(y = "Suitability index (FAO, 2000)", x = "Suitability index (Suit)") +
  theme_minimal(base_family = "serif", base_size = 18) +
  annotate("segment", x = 0, xend = 1, y = 0, yend = 1, color = "red3", size = 1, lty = "dashed") +
  geom_smooth(method = "lm", col = "red3", lty = "solid", se = F)+ 
  annotate("text", x = 0.02, y = 0.99, label = paste("y = ", round(coef_FAO[2], digits = 2), "x"), 
           hjust = 0, vjust = 1, color = "black", size = 4) + 
  annotate("text", x = 0.005, y = 0.95, label = TeX("R$^{2}$"), 
           hjust = 0, vjust = 1, color = "black", size = 4)  + 
  annotate("text", x = 0.035, y = 0.94, label = paste("=", round(summary(ols2)$r.squared, digits = 2)), 
           hjust = 0, vjust = 1, color = "black", size = 4) 
ggsave(filename = "./figures/TECH_FAO_suit_20.png", bg = "white", height = 6, width = 9, dpi = 300)



# FAO suitability 1990
ols3 <- lm(FAO_suit_90 ~ suit_30_raman, data = tech_val)
coef_FAO90 <- coef(ols3)

ggplot(data = tech_val, aes(x = suit_30_raman, y = FAO_suit_90)) + geom_point(alpha = 0.3) +
  labs(y = "Suitability index (FAO, 1990)", x = "Suitability index (Suit)") +
  theme_minimal(base_family = "serif", base_size = 18) +
  annotate("segment", x = 0, xend = 1, y = 0, yend = 1, color = "red3", size = 1, lty = "dashed") +
  geom_smooth(method = "lm", col = "red3", lty = "solid", se = F)+ 
  annotate("text", x = 0.02, y = 0.99, label = paste("y = ", round(coef_FAO90[2], digits = 2), "x"), 
           hjust = 0, vjust = 1, color = "black", size = 4) + 
  annotate("text", x = 0.005, y = 0.95, label = TeX("R$^{2}$"), 
           hjust = 0, vjust = 1, color = "black", size = 4)  + 
  annotate("text", x = 0.035, y = 0.94, label = paste("=", round(summary(ols3)$r.squared, digits = 2)), 
           hjust = 0, vjust = 1, color = "black", size = 4) 
ggsave(filename = "./figures/TECH_FAO_suit_90.png", bg = "white", height = 6, width = 9, dpi = 300)




### -------------------------------------------

# Evapotranspiartion 
# We compare with the measure of reference evapotranspiration by Zomer et al. 2022

ggplot(data = tech_val, aes(x = evapo_30, y = ET0_zomer)) + geom_point() +
  labs(y = "ET0 (Zomer et al. 2022)", x = "ET0 (1971 - 2000)") +
  theme_minimal(base_family = "serif", base_size = 15)
ggsave(filename = "./figures/TECH_evapo.png", height = 6, width = 9, dpi = 300, units = "in", bg = "white")


### -------------------------------------------




#########################################################
# Map differential 
load("./processed_data/europe.Rdata")

tech_val <- tech_val %>% mutate(raman_diff = suit_30_raman - raman_suit)
tech_val <- tech_val %>% mutate(FAO_diff = suit_30_FAO - FAO_suit)

tech_val_europe <- st_intersection(tech_val, europe)


tech_val_raman <- tm_shape(tech_val_europe) + tm_fill("raman_diff", breaks = seq(-1, 1, by = 0.5), palette = "RdYlGn", style = "cont", title = "Difference", showNA = F, colorNA = NULL) +
  tm_shape(tech_val_europe) + tm_fill("raman_diff", breaks = seq(-1, 1, by = 0.2), palette = "RdYlGn", legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif") + tm_shape(europe) + tm_borders()
tech_val_raman

tech_val_FAO <- tm_shape(tech_val_europe) + tm_fill("FAO_diff", breaks = seq(-1, 1, by = 0.5), palette = "RdYlGn", style = "cont", title = "Difference", showNA = F, colorNA = NULL) +
  tm_shape(tech_val_europe) + tm_fill("FAO_diff", breaks = seq(-1, 1, by = 0.2), palette = "RdYlGn", legend.show = F, showNA = F, colorNA = NULL) + 
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, fontfamily = "serif") + tm_shape(europe) + tm_borders()
tech_val_FAO


tmap_save(tech_val_FAO, filename = "./figures/tech_val_FAO.png")
tmap_save(tech_val_raman, filename = "./figures/tech_val_raman.png")



mean_diffs <- data.frame(Means = c("FAO", "Raman"), diff = c(mean(tech_val_europe$FAO_diff, na.rm = T), mean(tech_val_europe$raman_diff, na.rm = T)))
#>   cond rating.mean
#> 1    A -0.05775928
#> 2    B  0.87324927


ggplot(tech_val_europe, aes(x = FAO_diff)) +
  geom_histogram(aes(fill = "FAO"), binwidth = 0.1, alpha = 0.5, position = "identity") +
  geom_histogram(data = tech_val_europe, aes(x = raman_diff, fill = "Ramankutty et al. (2002)"),
                 binwidth = 0.1, alpha = 0.5, position = "identity") +
  labs(x = "Difference", y = "Count") +
  scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 250)) +
  scale_fill_manual(values = c("FAO" = "lemonchiffon4", "Ramankutty et al. (2002)" = "pink3")) +
  labs(fill = "Dataset:") +
theme_minimal(base_family = "serif", base_size = 18) + 
  theme(axis.line.x = element_line(size = 1),
        axis.ticks.x = element_line(size = 1), 
        legend.position = "top")
ggsave(filename = "./figures/histo_FAO_raman.png", bg = "white", device = "png", width=9, height=6)





