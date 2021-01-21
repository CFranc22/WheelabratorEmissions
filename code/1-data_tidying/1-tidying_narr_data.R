# Tidying data for 6 NCEP NARR variables during the study period 2018 to 2020
# Daily mean air temperature
# Daily mean U-wind component (10m)
# Daily mean V-wind component (10m)
# Daily mean albedo (10m)
# Daily mean accumulated total precipitation
# Daily mean relative humidity
# Daily mean planetary boundary layer height

#########################################################################
#Creating raster files of NARR data

# Daily mean air temperature
air.2018.raster =
  stack("data/raw/narr/air_temp_daily_mean/air.sfc.2018.nc")
air.2019.raster = 
  stack("data/raw/narr/air_temp_daily_mean/air.sfc.2019.nc")
air.2020.raster = 
  stack("data/raw/narr/air_temp_daily_mean/air.sfc.2020.nc")

air.stack <- raster::stack(air.2018.raster, air.2019.raster, air.2020.raster)

# Daily mean u-wind component
uwind.2018.raster = 
  stack("data/raw/narr/wind_u/uwnd.10m.2018.nc")
uwind.2019.raster = 
  stack("data/raw/narr/wind_u/uwnd.10m.2019.nc")
uwind.2020.raster = 
  stack("data/raw/narr/wind_u/uwnd.10m.2020.nc")

uwind.stack <- raster::stack(uwind.2018.raster, uwind.2019.raster, uwind.2020.raster)


# Daily mean v-wind component
vwind.2018.raster = 
  stack("data/raw/narr/wind_v/vwnd.10m.2018.nc")
vwind.2019.raster = 
  stack("data/raw/narr/wind_v/vwnd.10m.2019.nc")
vwind.2020.raster = 
  stack("data/raw/narr/wind_v/vwnd.10m.2020.nc")

vwind.stack <- raster::stack(vwind.2018.raster, vwind.2019.raster, vwind.2020.raster)


# Daily mean albedo
albedo.2018.raster = 
  stack("data/raw/narr/albedo/albedo.2018.nc")
albedo.2019.raster = 
  stack("data/raw/narr/albedo/albedo.2019.nc")
albedo.2020.raster = 
  stack("data/raw/narr/albedo/albedo.2020.nc")

albedo.stack <- raster::stack(albedo.2018.raster, albedo.2019.raster, albedo.2020.raster)


# Daily mean accumulated total precipitation
precip.2018.raster = 
  stack("data/raw/narr/accumulated_precip/apcp.2018.nc")
precip.2019.raster = 
  stack("data/raw/narr/accumulated_precip/apcp.2019.nc")
precip.2020.raster = 
  stack("data/raw/narr/accumulated_precip/apcp.2020.nc")

precip.stack <- raster::stack(precip.2018.raster, precip.2019.raster, precip.2020.raster)

# Daily mean relative humidity
rhum.2018.raster = 
  stack("data/raw/narr/relative_humidity/rhum.2m.2018.nc")
rhum.2019.raster = 
  stack("data/raw/narr/relative_humidity/rhum.2m.2019.nc")
rhum.2020.raster = 
  stack("data/raw/narr/relative_humidity/rhum.2m.2020.nc")

rhum.stack <- raster::stack(rhum.2018.raster, rhum.2019.raster, rhum.2020.raster)

# Daily mean planetery boundary layer height
pblh.2018.raster = 
  stack("data/raw/narr/planetary_boundary_layer_height/hpbl.2018.nc")
pblh.2019.raster = 
  stack("data/raw/narr/planetary_boundary_layer_height/hpbl.2019.nc")
pblh.2020.raster = 
  stack("data/raw/narr/planetary_boundary_layer_height/hpbl.2020.nc")

pblh.stack <- raster::stack(pblh.2018.raster, pblh.2019.raster, pblh.2020.raster)

# extracting NARR projection for future use, setting up environment
crs_projected <- st_crs(air.2018.raster)

##############################################################################

## creating SEARCH dataset (code available in separate .R file)

search_sites <- read.csv("data/interim/box_sites_coords.csv") %>%
 dplyr::select(box_ID, latitude, longitude)

## creating the SEARCH wireless multipollutant monitor site SF object, 
## datum/epsg = WGS 84 in the same projection as the narr datasets

search_sites_sf <- search_sites %>% 
  dplyr::select(box_ID, latitude, longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

search_sites_sf_lcc = st_transform(search_sites_sf, crs_projected)

##############################################################################
# Creating RDS files for each NARR raster dataset
  
# Creating air temperature dataset (2018 - 2019)

# extracting air temp raster values from search monitor points

########################
air.extract.2018 = raster::extract(air.2018.raster, search_sites_sf)

# exporting result as a dataframe
air.tibble.2018 = cbind(search_sites_sf, air.extract.2018) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

air.2018 <- air.tibble.2018 %>%
  ### converting from wide to long, creating the temperature column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "temperature") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

# creating the date column
air.2018 <- air.2018 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , air.2018$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

#converting from kelvin to celsius
air.2018$temperature = air.2018$temperature - 273.15

# extracting air temp raster values from search monitor points

########################

air.extract.2019 = raster::extract(air.2019.raster, search_sites_sf)

# exporting result as a dataframe
air.tibble.2019 = cbind(search_sites_sf, air.extract.2019) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

air.2019 <- air.tibble.2019 %>%
  ### converting from wide to long, creating the temperature column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "temperature") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

# creating the date column
air.2019 <- air.2019 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , air.2019$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

#converting from kelvin to celsius
air.2019$temperature = air.2019$temperature - 273.15

# extracting air temp raster values from search monitor points

########################

# extracting air temp raster values from search monitor points
air.extract.2020 = raster::extract(air.2020.raster, search_sites_sf)

# exporting result as a dataframe
air.tibble.2020 = cbind(search_sites_sf, air.extract.2020) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

air.2020 <- air.tibble.2020 %>%
  ### converting from wide to long, creating the temperature column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "temperature") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

# creating the date column
air.2020 <- air.2020 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , air.2020$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

#converting from kelvin to celsius
air.2020$temperature = air.2020$temperature - 273.15

########################

# merging together all the air temperature tibbles 2018-2020
narr.air <- full_join(air.2018, air.2019) %>% 
  full_join(., air.2020) 

# exports processed data
saveRDS(narr.air, "data/interim/narr_temperature.rds")

########################

# Creating wind dataset (2018 - 2020)

# extracting wind raster values from search monitor points

# extracting raster values from search monitor locations
uwind.extract.2018 = raster::extract(uwind.2018.raster, search_sites_sf)

#exporting result as a dataframe
uwind.tibble.2018 = cbind(search_sites_sf, uwind.extract.2018) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

### wide to long transformation
uwind.2018 <- uwind.tibble.2018 %>%
  ### converting from wide to long, creating the temperature column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "u_wind") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

uwind.2018 <- uwind.2018 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , uwind.2018$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

# extracting raster values from search monitor locations
vwind.extract.2018 = raster::extract(vwind.2018.raster, search_sites_sf)

#exporting result as a dataframe
vwind.tibble.2018 = cbind(search_sites_sf, vwind.extract.2018) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

### wide to long transformation
vwind.2018 <- vwind.tibble.2018 %>%
  ### converting from wide to long, creating the temperature column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "v_wind") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

vwind.2018 <- vwind.2018 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , vwind.2018$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
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
  mutate(wind_speed = sqrt(u_wind^2+v_wind^2))

########################

# extracting raster values from search monitor locations
uwind.extract.2019 = raster::extract(uwind.2019.raster, search_sites_sf)

#exporting result as a dataframe
uwind.tibble.2019 = cbind(search_sites_sf, uwind.extract.2019) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

### wide to long transformation
uwind.2019 <- uwind.tibble.2019 %>%
  ### converting from wide to long, creating the temperature column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "u_wind") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

uwind.2019 <- uwind.2019 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , uwind.2019$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

# extracting raster values from search monitor locations
vwind.extract.2019 = raster::extract(vwind.2019.raster, search_sites_sf)

#exporting result as a dataframe
vwind.tibble.2019 = cbind(search_sites_sf, vwind.extract.2019) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

### wide to long transformation
vwind.2019 <- vwind.tibble.2019 %>%
  ### converting from wide to long, creating the temperature column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "v_wind") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

vwind.2019 <- vwind.2019 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , vwind.2019$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
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
  mutate(wind_speed = sqrt(u_wind^2+v_wind^2))

########################

# extracting raster values from search monitor locations
uwind.extract.2020 = raster::extract(uwind.2020.raster, search_sites_sf)

#exporting result as a dataframe
uwind.tibble.2020 = cbind(search_sites_sf, uwind.extract.2020) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

### wide to long transformation
uwind.2020 <- uwind.tibble.2020 %>%
  ### converting from wide to long, creating the temperature column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "u_wind") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

uwind.2020 <- uwind.2020 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , uwind.2020$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

# extracting raster values from search monitor locations
vwind.extract.2020 = raster::extract(vwind.2020.raster, search_sites_sf)

#exporting result as a dataframe
vwind.tibble.2020 = cbind(search_sites_sf, vwind.extract.2020) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

### wide to long transformation
vwind.2020 <- vwind.tibble.2020 %>%
  ### converting from wide to long, creating the temperature column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "v_wind") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

vwind.2020 <- vwind.2020 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , vwind.2020$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
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
  mutate(wind_speed = sqrt(u_wind^2+v_wind^2))

########################

# merging together all the wind tibbles 2018-2020
narr.wind <- full_join(wind.2018, wind.2019) %>% 
  full_join(., wind.2020) 

# exports processed data
saveRDS(narr.wind, "data/interim/narr_wind.rds")

########################

# Creating abedo dataset (2018 - 2020)

# extracting albedo raster values from search monitor points
albedo.extract.2018 = raster::extract(albedo.2018.raster, search_sites_sf)

# exporting result as a dataframe
albedo.tibble.2018 = cbind(search_sites_sf, albedo.extract.2018) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

albedo.2018 <- albedo.tibble.2018 %>%
  ### converting from wide to long, creating the albedo column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "albedo") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

# creating the date column
albedo.2018 <- albedo.2018 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , albedo.2018$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

# extracting albedo raster values from search monitor points
albedo.extract.2019 = raster::extract(albedo.2019.raster, search_sites_sf)

# exporting result as a dataframe
albedo.tibble.2019 = cbind(search_sites_sf, albedo.extract.2019) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

albedo.2019 <- albedo.tibble.2019 %>%
  ### converting from wide to long, creating the albedo column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "albedo") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

# creating the date column
albedo.2019 <- albedo.2019 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , albedo.2019$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

# extracting albedo raster values from search monitor points
albedo.extract.2020 = raster::extract(albedo.2020.raster, search_sites_sf)

# exporting result as a dataframe
albedo.tibble.2020 = cbind(search_sites_sf, albedo.extract.2020) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

albedo.2020 <- albedo.tibble.2020 %>%
  ### converting from wide to long, creating the albedo column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "albedo") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

# creating the date column
albedo.2020 <- albedo.2020 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , albedo.2020$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2)

########################

# merging together all the albedo tibbles 2018-2020
narr.albedo <- full_join(albedo.2018, albedo.2019) %>% 
  full_join(., albedo.2020) 

# exports processed data
saveRDS(narr.albedo, "data/interim/narr_albedo.rds")

########################

# Creating precipitation dataset (2018 - 2020)

# extracting precip raster values from search monitor points
precip.extract.2018 = raster::extract(precip.2018.raster, search_sites_sf)

# exporting result as a dataframe
precip.tibble.2018 = cbind(search_sites_sf, precip.extract.2018) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

precip.2018 <- precip.tibble.2018 %>%
  ### converting from wide to long, creating the precip column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "precip") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

# creating the date column
precip.2018 <- precip.2018 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , precip.2018$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

# extracting precip raster values from search monitor points
precip.extract.2019 = raster::extract(precip.2019.raster, search_sites_sf)

# exporting result as a dataframe
precip.tibble.2019 = cbind(search_sites_sf, precip.extract.2019) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

precip.2019 <- precip.tibble.2019 %>%
  ### converting from wide to long, creating the precip column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "precip") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

# creating the date column
precip.2019 <- precip.2019 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , precip.2019$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

# extracting precip raster values from search monitor points
precip.extract.2020 = raster::extract(precip.2020.raster, search_sites_sf)

# exporting result as a dataframe
precip.tibble.2020 = cbind(search_sites_sf, precip.extract.2020) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

precip.2020 <- precip.tibble.2020 %>%
  ### converting from wide to long, creating the precip column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "precip") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

# creating the date column
precip.2020 <- precip.2020 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , precip.2020$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

########################

# merging together all the precipitation tibbles 2018-2020
narr.precip <- full_join(precip.2018, precip.2019) %>% 
  full_join(., precip.2020) 

# exports processed data
saveRDS(narr.precip, "data/interim/narr_precip.rds")

########################

# Creating relative humidity dataset (2018 - 2020)

# extracting rhum raster values from search monitor points
rhum.extract.2018 = raster::extract(rhum.2018.raster, search_sites_sf)

# exporting result as a dataframe
rhum.tibble.2018 = cbind(search_sites_sf, rhum.extract.2018) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

rhum.2018 <- rhum.tibble.2018 %>%
  ### converting from wide to long, creating the rhum column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "rhum") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

# creating the date column
rhum.2018 <- rhum.2018 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , rhum.2018$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

# extracting rhum raster values from search monitor points
rhum.extract.2019 = raster::extract(rhum.2019.raster, search_sites_sf)

# exporting result as a dataframe
rhum.tibble.2019 = cbind(search_sites_sf, rhum.extract.2019) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

rhum.2019 <- rhum.tibble.2019 %>%
  ### converting from wide to long, creating the rhum column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "rhum") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

# creating the date column
rhum.2019 <- rhum.2019 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , rhum.2019$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

# extracting rhum raster values from search monitor points
rhum.extract.2020 = raster::extract(rhum.2020.raster, search_sites_sf)

# exporting result as a dataframe
rhum.tibble.2020 = cbind(search_sites_sf, rhum.extract.2020) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

rhum.2020 <- rhum.tibble.2020 %>%
  ### converting from wide to long, creating the rhum column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "rhum") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

# creating the date column
rhum.2020 <- rhum.2020 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , rhum.2020$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

########################

# merging together all the rhum tibbles 2018-2020
narr.rhum <- full_join(rhum.2018, rhum.2019) %>% 
  full_join(., rhum.2020) 

# exports processed data
saveRDS(narr.rhum, "data/interim/narr_rhum.rds")

########################

# Creating planetary boundary layer height dataset (2018 - 2020)

# extracting planetary boundary layer height raster values from search monitor points
pblh.extract.2018 = raster::extract(pblh.2018.raster, search_sites_sf)

# exporting result as a dataframe
pblh.tibble.2018 = cbind(search_sites_sf, pblh.extract.2018) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

pblh.2018 <- pblh.tibble.2018 %>%
  ### converting from wide to long, creating the pblh column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "pblh") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

# creating the date column
pblh.2018 <- pblh.2018 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , pblh.2018$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

# extracting planetary boundary layer height raster values from search monitor points
pblh.extract.2019 = raster::extract(pblh.2019.raster, search_sites_sf)

# exporting result as a dataframe
pblh.tibble.2019 = cbind(search_sites_sf, pblh.extract.2019) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

pblh.2019 <- pblh.tibble.2019 %>%
  ### converting from wide to long, creating the pblh column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "pblh") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

# creating the date column
pblh.2019 <- pblh.2019 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , pblh.2019$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

# extracting planetary boundary layer height raster values from search monitor points
pblh.extract.2020 = raster::extract(pblh.2020.raster, search_sites_sf)

# exporting result as a dataframe
pblh.tibble.2020 = cbind(search_sites_sf, pblh.extract.2020) %>% 
  as_tibble() %>%
  dplyr::select(-geometry)

pblh.2020 <- pblh.tibble.2020 %>%
  ### converting from wide to long, creating the pblh column, and naming the dates column "day_of_year" while creating a new column date column without the x character
  pivot_longer(-box_ID,
               names_to  = "day_of_year",
               values_to = "pblh") %>% 
  mutate(date1 = str_sub(day_of_year, 2, -1))

# creating the date column
pblh.2020 <- pblh.2020 %>% 
  #creating a new date column that replaces "." with "/"
  mutate(date2 = paste(gsub("[.]", "/" , pblh.2020$date1))) %>% 
  # date column -> date column type format
  mutate(date = as.Date(date2, format = "%Y/%m/%d")) %>% 
  #removing excess column to create finalized date column
  dplyr::select(-day_of_year, -date1, -date2) 

########################

# merging together all the pblh tibbles 2018-2020
narr.pblh <- full_join(pblh.2018, pblh.2019) %>% 
  full_join(., pblh.2020) 

# exports processed data
saveRDS(narr.pblh, "data/interim/narr_pblh.rds")

#########################################################################

# creating a super narr dataset!

narr <- full_join (narr.air, narr.wind) %>% 
  full_join(., narr.albedo) %>%
  full_join(., narr.precip) %>% 
  full_join(., narr.rhum) %>%
  full_join(., narr.pblh) 

# creating a super NARR + search (lat/lon) dataset!

dat <- full_join(narr, search_sites)

saveRDS(dat, "data/interim/narr_search.rds")
