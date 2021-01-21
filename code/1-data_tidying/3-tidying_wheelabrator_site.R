## Creating the Wheelabrator 12-km radius, with 1-km annuli rings 

## creating a dataframe using Wheelabrator lon + lat coordinates 

x = -76.629099
y = 39.270195

wheelabrator_address <- data.frame(x,y)


## converting wheelabrator data fram to sf object
wheelabrator_sf <- wheelabrator_address %>%
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  mutate(site = "wheelabrator incinerator")

## projecting wheelabrator sf object in sae projection as narr data
wheelabrator_sf_lcc = st_transform(wheelabrator_sf, crs_projected)



# creating wheelabrator + wind direction dataset  
# extracting raster values from wheelabrator location
uwind.extract.2018 = raster::extract(uwind.2018.raster, wheelabrator_sf)

#exporting result as a dataframe
uwind.tibble.2018 = cbind(wheelabrator_sf, uwind.extract.2018) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

### wide to long transformation
uwind.2018 <- uwind.tibble.2018 %>%
  ### converting from wide to long, creating the temperature column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-site,
               names_to  = "day_of_year",
               values_to = "u_wind") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1)) %>% 
  mutate(u_wind = as.numeric(u_wind))

uwind.2018 <- uwind.2018 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , uwind.2018$date1))) %>% 
  # date column -> date column type format
  mutate(date = paste(as.Date(date2, format = "%Y/%m/%d"))) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

vwind.extract.2018 = raster::extract(vwind.2018.raster, wheelabrator_sf)

#exporting result as a dataframe
vwind.tibble.2018 = cbind(wheelabrator_sf, vwind.extract.2018) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

### wide to long transformation
vwind.2018 <- vwind.tibble.2018 %>%
  ### converting from wide to long, creating the temperature column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-site,
               names_to  = "day_of_year",
               values_to = "v_wind") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1)) %>% 
  mutate(v_wind = as.numeric(v_wind))
 
vwind.2018 <- vwind.2018 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , vwind.2018$date1))) %>% 
  # date column -> date column type format
  mutate(date = paste(as.Date(date2, format = "%Y/%m/%d"))) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

########################

# merging the u-wind and v-wind components
winds_joined2018 = inner_join(uwind.2018, vwind.2018)

# calculating wind direction and wind speed
wind.2018 <- winds_joined2018 %>% 
  #creating a new column for wind direction using arctan(u,v) 
  mutate(wind_direction = 
           # (180/pi) <- converts to to degrees from radians
           # (+ 180) <- makes degree range (0, 360) instead of (-180,180)
           paste(((180/pi)*(atan2(u_wind,v_wind))+180))) %>% 
  mutate(wind_speed = sqrt(u_wind^2+v_wind^2)) %>% 
  mutate(wind_direction = as.numeric(wind_direction))

# extracting raster values from search monitor locations
uwind.extract.2019 = raster::extract(uwind.2019.raster, wheelabrator_sf)

#exporting result as a dataframe
uwind.tibble.2019 = cbind(wheelabrator_sf, uwind.extract.2019) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

### wide to long transformation
uwind.2019 <- uwind.tibble.2019 %>%
  ### converting from wide to long, creating the temperature column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-site,
               names_to  = "day_of_year",
               values_to = "u_wind") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1)) %>% 
  mutate(u_wind = as.numeric(u_wind))

uwind.2019 <- uwind.2019 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , uwind.2019$date1))) %>% 
  # date column -> date column type format
  mutate(date = paste(as.Date(date2, format = "%Y/%m/%d"))) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

# extracting raster values from search monitor locations
vwind.extract.2019 = raster::extract(vwind.2019.raster, wheelabrator_sf)

#exporting result as a dataframe
vwind.tibble.2019 = cbind(wheelabrator_sf, vwind.extract.2019) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

### wide to long transformation
vwind.2019 <- vwind.tibble.2019 %>%
  ### converting from wide to long, creating the temperature column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-site,
               names_to  = "day_of_year",
               values_to = "v_wind") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1)) %>% 
  mutate(v_wind = as.numeric(v_wind))

vwind.2019 <- vwind.2019 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , vwind.2019$date1))) %>% 
  # date column -> date column type format
  mutate(date = paste(as.Date(date2, format = "%Y/%m/%d"))) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

########################

# merging the u-wind and v-wind components
winds_joined2019 = inner_join(uwind.2019, vwind.2019)

# calculating wind direction and wind speed
wind.2019 <- winds_joined2019 %>% 
  #creating a new column for wind direction using arctan(u,v) 
  mutate(wind_direction = 
           # (180/pi) <- converts to to degrees from radians
           # (+ 180) <- makes degree range (0, 360) instead of (-180,180)
           paste(((180/pi)*(atan2(u_wind,v_wind))+180))) %>% 
  mutate(wind_speed = sqrt(u_wind^2+v_wind^2)) %>% 
  mutate(wind_direction = as.numeric(wind_direction))


# extracting raster values from search monitor locations
uwind.extract.2020 = raster::extract(uwind.2020.raster, wheelabrator_sf)

#exporting result as a dataframe
uwind.tibble.2020 = cbind(wheelabrator_sf, uwind.extract.2020) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

### wide to long transformation
uwind.2020 <- uwind.tibble.2020 %>%
  ### converting from wide to long, creating the temperature column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-site,
               names_to  = "day_of_year",
               values_to = "u_wind") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1)) %>% 
  mutate(u_wind = as.numeric(u_wind))

uwind.2020 <- uwind.2020 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , uwind.2020$date1))) %>% 
  # date column -> date column type format
  mutate(date = paste(as.Date(date2, format = "%Y/%m/%d"))) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

# extracting raster values from search monitor locations
vwind.extract.2020 = raster::extract(vwind.2020.raster, wheelabrator_sf)

#exporting result as a dataframe
vwind.tibble.2020 = cbind(wheelabrator_sf, vwind.extract.2020) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

### wide to long transformation
vwind.2020 <- vwind.tibble.2020 %>%
  ### converting from wide to long, creating the temperature column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-site,
               names_to  = "day_of_year",
               values_to = "v_wind") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1)) %>% 
  mutate(v_wind = as.numeric(v_wind))

vwind.2020 <- vwind.2020 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , vwind.2020$date1))) %>% 
  # date column -> date column type format
  mutate(date = paste(as.Date(date2, format = "%Y/%m/%d"))) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

########################

# merging the u-wind and v-wind components
winds_joined2020 = inner_join(uwind.2020, vwind.2020)

# calculating wind direction and wind speed
wind.2020 <- winds_joined2020 %>% 
  #creating a new column for wind direction using arctan(u,v) 
  mutate(wind_direction = 
           # (180/pi) <- converts to to degrees from radians
           # (+ 180) <- makes degree range (0, 360) instead of (-180,180)
           paste(((180/pi)*(atan2(u_wind,v_wind))+180))) %>% 
  mutate(wind_speed = sqrt(u_wind^2+v_wind^2))  %>% 
  mutate(wind_direction = as.numeric(wind_direction))

# merging together all the wind tibbles 2018-2020
wheelabrator_wind <- full_join(wind.2018, wind.2019) %>% 
  full_join(., wind.2020) 

wheelabrator <- merge(wheelabrator_sf, wheelabrator_wind)

# exports processed data
saveRDS(wheelabrator_wind, "data/interim/wheelabrator_wind.rds")








