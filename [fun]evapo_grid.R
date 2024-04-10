
Reference_Evapo <- function(TEMPER, TMAX, TMIN, WIND, HUM, SUN, LATITUDE, ELEVATION){
  
  for (i in 1:365){

    pi <- 22/7
    # Latent heat of vaporization
    Lambda1 <- 2.501-0.002361*TEMPER
    
    # Atmospheric pressure (kPa) at elevation A
    P <- 101.3*(((293-0.0065*ELEVATION)/293)^5.256)
    
    # Psychrometric constant
    gamma <- 0.0016286*(P/Lambda1)
    
    # Aerodynamic resistance:
    r_a <- 208/WIND 
    
    # Crop canopy resistance
    r_c <- 100/(0.5*(2.88))
    
    
    # Modified psychrometric constant
    
    gamma_star <- gamma*(1 + (r_c/r_a))
    
    
    # Saturation vapor pressure ea for given temperatures T min and T max
    
    e_ax <- 0.6108*exp((17.27*TMAX)/(237.3 + TMAX)) 
    
    e_am <- 0.6108*exp((17.27*TMIN)/(237.3 + TMIN))
    
    e_a <- 0.5*(e_ax + e_am)
    
    
    # Vapor pressure at dew point, e_d
    
    e_d <- (HUM/100)*(0.5/((1/e_ax) + (1/e_am)))
    
    
    # Slope of vapor pressure curve, C, for given temperatures T_max, and T_min
    C_x <- (4096*e_ax)/(237.3 + TMAX)^2
    C_m <- (4096*e_am)/(237.3 + TMIN)^2
    C <- (C_x + C_m)
    
    # Latitude experssed in rad 
    Phi <- LATITUDE*(pi/180)
    
    #Solar declination (rad)
    
    seq_year <- seq(1, 365, by = 1)
    #delta <- rep(list(0), 365)
    # Declination can also be given in radians by the Spencer formula (Spenser, 1971)
    # https://www.sciencedirect.com/topics/engineering/solar-declination
    
    Tau[[i]] <- ((2*pi*seq_year[i])-1)/365
    
    delta[[i]] <- 0.006918-0.339912*cos(Tau[[i]]) + 0.070257*sin(2*Tau[[i]]) - 0.006758*cos(2*Tau[[i]])+0.000907*sin(Tau[[i]])-0.002697*cos(3*Tau[[i]]) + 0.00148*sin(3*Tau[[i]])
    
    # Relative distance Earth to sun
    #d <- (1 + 0.033*cos(((2*pi)/365)*170))
    d[[i]] <- (1 + 0.033*cos(((2*pi)/365)*seq_year[i]))
    
    # Sunset hour angle (rad)
    # ARCCOS defined only between -1 and +1, put max and min threshold!!
    psi[[i]] <- acos(-(tan(Phi))*tan(delta[[i]]))
    
    # Extraterrestrial radiation
    Ra[[i]] <- (37.586*d[[i]]*(psi[[i]]*sin(Phi)*sin(delta[[i]])+cos(Phi)*cos(delta[[i]])*sin(psi[[i]])))
    
    # Maximum daylight hours
    
    DL[[i]] <- (24/pi)*psi[[i]]
    
    # Short wave radiation 
    
    Rs[[i]] <- (0.25 + 0.5*SUN/DL[[i]])*Ra[[i]]
    
    # Net incoming short-wave radiation 
    
    Rns[[i]] <- 0.77*Rs[[i]]
    
    # Net outgoing long wave radiation Rnl
    
    Rnl[[i]] <- 4.903*10^(-9)*(0.1 + 0.9*(SUN/DL[[i]]))*(0.34-0.139*sqrt(e_d))*((273.16 + TMAX)^4 + (273.16 + TMIN)^4)/2
    
    # Net radiation flux at surface
    Rn[[i]] <- Rns[[i]] - Rnl[[i]]
    
    # Soil heat flux
    
    # G <- 0.14*(as.matrix(TEMPER)-as.matrix(TEMPERpre))
    
    # The aerodynamic term
    ETar <- gamma/(C+gamma_star)*(900/(TEMPER+273))*WIND*(e_a-e_d)
    
    
    # The radiation term 
    
    # ETra <- C/(C + gamma_star)*(Rn-G)*(1/Lambda1)
    # ETra without the "G" using previous temperature
    ETra[[i]] <- C/(C + gamma_star)*(Rn[[i]])*(1/Lambda1)
    
    ET0[[i]] <- ETar + ETra[[i]]
    
    ET0_grid[[i]]$ET0 <- ET0[[i]]
    ET0_grid[[i]] <- ET0_grid[[i]] %>% st_drop_geometry()
  }
  
  # The daily evapo have been generated, we can now bind everything together 
  ET0_bind <- list.rbind(ET0_grid)
  
  ET0_bind

}

