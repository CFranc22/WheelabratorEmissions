## generalized function to assess exposure by indicating whether a counting the number of
## upwind or downwind wells in 1-km radius annuli within 15 km of the site,
## using NARR data for daily mean wind direction

# takes monitor coordinates ('monitor') as an sf object, generates 1-km annuli
# around the monitor out to 15 km, and counts the number of well sites, both
# in the preproduction and production stages, within each annulus

assessExposureAnnuliCountWind <- function(monitor, 
                                          wheelabrator,
                                          exp_variable_root){
  
  #.........................................................................
  # prepares the monitor dataset
  
  # captures date for feeding into the function below
  monitor_date   <- monitor$date
  monitor_lat    <- unlist(map(monitor$geometry, 1))
  monitor_long   <- unlist(map(monitor$geometry, 2))
  wind_direction <- monitor$wind_direction

  
  #.........................................................................
  # if there are wells that have dates that intersect with the monitor interval,
  # counts and stores number of well sites within each annulus; otherwise, we
  # assign 0 to all annuli wihtout annuli functions to improve efficiency;
  # the variable names are flexible name to account for new, active, idle, or 
  # abandoned wells
    
 # monitor <- monitor %>% select(Site_Number) #site
  
  #.........................................................................
  # prepares wells data
  
  # generates 12 km buffer as a mask around monitor coordinates
  monitor_mask <- monitor %>% 
    st_transform(crs_projected) %>%
    st_buffer(dist = 12000) %>%
    st_transform(crs = 4326)
  
    #.......................................................................
    # makes annuli around the maternal residence coordinates in the 'monitor' data
    annulus0to1 <- monitor %>%
      st_transform(crs_projected) %>%
      st_buffer(dist = 1000) %>%
      st_transform(crs = 4326)
    annulus1to2 <- monitor %>% 
      st_transform(crs_projected) %>%
      st_buffer(dist = 2000) %>%
      st_transform(crs = 4326)
    annulus2to3 <- monitor %>% 
      st_transform(crs_projected) %>%
      st_buffer(dist = 3000) %>%
      st_transform(crs = 4326)
    annulus3to4 <- monitor %>% 
      st_transform(crs_projected) %>%
      st_buffer(dist = 4000) %>%
      st_transform(crs = 4326)
    annulus4to5 <- monitor %>% 
      st_transform(crs_projected) %>%
      st_buffer(dist = 5000) %>%
      st_transform(crs = 4326)
    annulus5to6 <- monitor %>% 
      st_transform(crs_projected) %>%
      st_buffer(dist = 6000) %>%
      st_transform(crs = 4326)
    annulus6to7 <- monitor %>% 
      st_transform(crs_projected) %>%
      st_buffer(dist = 7000) %>%
      st_transform(crs = 4326)
    annulus7to8 <- monitor %>% 
      st_transform(crs_projected) %>%
      st_buffer(dist = 8000) %>%
      st_transform(crs = 4326)
    annulus8to9 <- monitor %>% 
      st_transform(crs_projected) %>%
      st_buffer(dist = 9000) %>%
      st_transform(crs = 4326)
    annulus9to10 <- monitor %>% 
      st_transform(crs_projected) %>%
      st_buffer(dist = 10000) %>%
      st_transform(crs = 4326)
    annulus10to11 <- monitor %>% 
      st_transform(crs_projected) %>%
      st_buffer(dist = 11000) %>%
      st_transform(crs = 4326)
    annulus11to12 <- monitor %>% 
      st_transform(crs_projected) %>%
      st_buffer(dist = 12000) %>%
      st_transform(crs = 4326)
    
    #.........................................................................
    ## creates a "wedge" facing towards the upwind origin point (point0)
    
    # captures monitor point from the coordinates
    monitor_point <- matrix(c(monitor_lat, monitor_long),
                            ncol = 2, byrow = TRUE)
    colnames(monitor_point) <- c('lat', 'long') 
    monitor_point <- as_tibble(monitor_point) %>%
      mutate(lat  = as.numeric(lat), long = as.numeric(long))
    
    # calculating upwind vector and creating a matrix
    upwind_point0 <- matrix(c((monitor_lat  + ((20/110.574) *  
                                                 cos(wind_direction * (pi/180)))),
                              (monitor_long + ((20/110.574) * 
                                                 sin(wind_direction * (pi/180))))),
                            ncol = 2, byrow = TRUE)
    colnames(upwind_point0) <- c('lat', 'long') 
    upwind_point0 <- as_tibble(upwind_point0) %>%
      mutate(lat  = as.numeric(lat), long = as.numeric(long))
    
    # calculating upwind points on either side of the wind direction
    # first point
    upwind_point1 <-
      matrix(c((monitor_lat  + (((20 / 110.574) / cos(45 * (pi/180))) * 
                                  cos((wind_direction + 45) * (pi/180)))),
               (monitor_long + (((20 / 110.574) / cos(45 * (pi/180))) *
                                  sin((wind_direction + 45) * (pi/180))))),
             ncol = 2, byrow = TRUE)
    colnames(upwind_point1) <- c('lat', 'long')
    upwind_point1 <- as_tibble(upwind_point1) %>%
      mutate(lat  = as.numeric(lat), long = as.numeric(long))
    
    # second point
    upwind_point2 <- 
      matrix(c((monitor_lat + (((20 / 110.574) / cos(45 * (pi/180))) *
                                 cos((wind_direction - 45) * (pi/180)))),
               (monitor_long + (((20 / 110.574) / cos(45 * (pi/180)))*
                                  sin((wind_direction - 45) * (pi/180))))),
             ncol = 2, byrow = TRUE)
    colnames(upwind_point2) <- c('lat', 'long')
    upwind_point2 <- as_tibble(upwind_point2) %>%
      mutate(lat  = as.numeric(lat), long = as.numeric(long))
    
    # makes the first half of the "wedge"
    wedge1 <- full_join(upwind_point1, upwind_point0) %>% 
      full_join(y = monitor_point)
    wedge1 <- st_as_sf(wedge1, coords = c("lat", "long"))
    st_crs(wedge1) <- 4326
    wedge1 <- wedge1 %>%
      st_coordinates() %>%
      st_multipoint() %>% 
      st_cast("POLYGON") %>% 
      st_sfc(crs = 4326)
    
    # makes the second half of the "wedge"
    wedge2 <- full_join(upwind_point2, upwind_point0) %>% 
      full_join(y = monitor_point) 
    wedge2 <- st_as_sf(wedge2, coords = c("lat", "long"))
    st_crs(wedge2) <- 4326
    wedge2 <- wedge2 %>% 
      st_coordinates() %>%
      st_multipoint() %>% 
      st_cast("POLYGON") %>%
      st_sfc(crs = 4326)
    
    # combines the two halves into the full wedge
    upwind_wedge <- st_union(wedge1, wedge2)
    
    #.........................................................................
    
    # finalizes annuli by successively clipping differences in reverse order
    
    annulus11to12 <- st_difference(annulus11to12, annulus10to11) %>%
      st_intersection(upwind_wedge)
    annulus10to11 <- st_difference(annulus10to11, annulus9to10) %>%
      st_intersection(upwind_wedge)
    annulus9to10  <- st_difference(annulus9to10,  annulus8to9) %>%
      st_intersection(upwind_wedge)
    annulus8to9   <- st_difference(annulus8to9,   annulus7to8) %>%
      st_intersection(upwind_wedge)
    annulus7to8   <- st_difference(annulus7to8,   annulus6to7) %>%
      st_intersection(upwind_wedge)
    annulus6to7   <- st_difference(annulus6to7,   annulus5to6) %>%
      st_intersection(upwind_wedge)
    annulus5to6   <- st_difference(annulus5to6,   annulus4to5) %>%
      st_intersection(upwind_wedge)
    annulus4to5   <- st_difference(annulus4to5,   annulus3to4) %>%
      st_intersection(upwind_wedge)
    annulus3to4   <- st_difference(annulus3to4,   annulus2to3) %>%
      st_intersection(upwind_wedge)
    annulus2to3   <- st_difference(annulus2to3,   annulus1to2) %>%
      st_intersection(upwind_wedge)
    annulus1to2   <- st_difference(annulus1to2,   annulus0to1) %>%
      st_intersection(upwind_wedge)
    annulus0to1   <- st_intersection(annulus0to1, upwind_wedge)
    
    # counts wells within each 1-km annulus
    monitor <- monitor %>%  
      mutate(!!as.name(paste(exp_variable_root, sep = "", "0to1km")) :=   
               sum(unlist(st_intersects(wheelabrator, annulus0to1))),
             !!as.name(paste(exp_variable_root, sep = "", "1to2km")) := 
               sum(unlist(st_intersects(wheelabrator, annulus1to2))),
             !!as.name(paste(exp_variable_root, sep = "", "2to3km")) := 
               sum(unlist(st_intersects(wheelabrator, annulus2to3))),
             !!as.name(paste(exp_variable_root, sep = "", "3to4km")) := 
               sum(unlist(st_intersects(wheelabrator, annulus3to4))),
             !!as.name(paste(exp_variable_root, sep = "", "4to5km")) := 
               sum(unlist(st_intersects(wheelabrator, annulus4to5))),
             !!as.name(paste(exp_variable_root, sep = "", "5to6km")) := 
               sum(unlist(st_intersects(wheelabrator, annulus5to6))),
             !!as.name(paste(exp_variable_root, sep = "", "6to7km")) := 
               sum(unlist(st_intersects(wheelabrator, annulus6to7))),
             !!as.name(paste(exp_variable_root, sep = "", "7to8km")) := 
               sum(unlist(st_intersects(wheelabrator, annulus7to8))),
             !!as.name(paste(exp_variable_root, sep = "", "8to9km")) := 
               sum(unlist(st_intersects(wheelabrator, annulus8to9))),
             !!as.name(paste(exp_variable_root, sep = "", "9to10km")) := 
               sum(unlist(st_intersects(wheelabrator, annulus9to10))),
             !!as.name(paste(exp_variable_root, sep = "", "10to11km")) := 
               sum(unlist(st_intersects(wheelabrator, annulus10to11))),
             !!as.name(paste(exp_variable_root, sep = "", "11to12km")) := 
               sum(unlist(st_intersects(wheelabrator, annulus11to12)))) %>%
      as_tibble() %>% 
#      select(-geometry) %>%
      mutate(date = monitor_date)
    
  
  
  #.........................................................................
  # returns the processed exposure data
  
  return(monitor)
  
}

##============================================================================##