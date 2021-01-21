## tidying wireless multipollutant monitor data 
## to characterize periods of function

## creating SEARCH pm 2.5 dataset using relevant columns created in 
## the revised excel file

search_pollutants <- read.csv("data/interim/box_site_pm25.csv") %>%
  dplyr::select(site, box_ID, date, latitude,	longitude, pm25_raw, 
                pm25_cor, year, month, dow)

## creating a new dataframe with pm 2.5 daily averages
pm_raw_avg <- aggregate(pm25_raw ~ box_ID + date, search_pollutants, mean)
pm_cor_avg <- aggregate(pm25_cor ~ box_ID + date, search_pollutants, mean)

## creating a dataframe based on pm 2.5 daily observations for each monitor-day
search_pollutants_daily <- search_pollutants %>% 
  dplyr::select(-pm25_raw, -pm25_cor) %>% 
  left_join(pm_raw_avg,by = c("box_ID", "date")) %>% 
  left_join(pm_cor_avg,by = c("box_ID", "date")) %>% 
  mutate(pm_raw_avg = pm25_raw) %>% 
  mutate(pm_cor_avg = pm25_cor) %>% 
  dplyr::select(-pm_raw_avg, -pm_cor_avg)

search_pollutants_daily <- unique(search_pollutants_daily)

# converts date column to date class
search_pollutants_daily$date <-
  search_pollutants_daily$date %>%
  as.Date(format = "%Y-%m-%d")  # forms date

saveRDS(search_pollutants_daily, file = "data/interim/search_daily_pm25.RDS")

## projecting SEARCH dataset to match NARR data
## creating the SEARCH monitor-day pollutant 
##SF object, datum/epsg = WGS 84 in the same projection as the narr datasets

search_pollutants_sf <- search_pollutants_daily %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

search_pollutants_sf_lcc = st_transform(search_sites_sf, crs_projected)

## creating SEARCH coordinates dataset 

search_sites <- read.csv("data/interim/box_sites_coords.csv") %>%
  dplyr::select(box_ID, latitude, longitude)

## creating the SEARCH wireless multipollutant monitor locations site SF object, 
## datum/epsg = WGS 84 in the same projection as the narr datasets

search_sites_sf <- search_sites %>% 
  dplyr::select(box_ID, latitude, longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

search_sites_sf_lcc = st_transform(search_sites_sf, crs_projected)