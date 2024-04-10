# Figure/plots for Technical validation

rm(list = ls())
gc()

setwd("C:/Users/DylanPhilippe/OneDrive - Wyss Academy for Nature/Documents/GitHub/Agricultural_Suitability")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tmap, dplyr, zoo, ggplot2, sandwich,latex2exp)

options(scipen = 999) # This option allows to remove the scientific notation

sf::sf_use_s2(FALSE)


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





