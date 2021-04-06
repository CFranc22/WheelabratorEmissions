# getting to know the results of the exposure assessment 

results <- readRDS("data/processed/assessment/NO2_MDE_AQS_daily_mean_90.rds")

## creating a binary variable for exposed monitor-days
results <- mutate(results, exposed = ifelse(upwind_from_search_monitors_0to1km > 0, "1",
                                            ifelse(upwind_from_search_monitors_1to2km > 0, "1",
                                                   ifelse(upwind_from_search_monitors_2to3km > 0, "1",
                                                          ifelse(upwind_from_search_monitors_3to4km > 0, "1",
                                                                 ifelse(upwind_from_search_monitors_4to5km > 0, "1",
                                                                        ifelse(upwind_from_search_monitors_5to6km > 0, "1",
                                                                               ifelse(upwind_from_search_monitors_6to7km > 0, "1",
                                                                                      ifelse(upwind_from_search_monitors_7to8km > 0, "1",
                                                                                             ifelse(upwind_from_search_monitors_8to9km > 0, "1",
                                                                                                    ifelse(upwind_from_search_monitors_9to10km > 0, "1",
                                                                                                           ifelse(upwind_from_search_monitors_10to11km > 0, "1",
                                                                                                                  ifelse(upwind_from_search_monitors_11to12km > 0, "1",
                                                                                                                         ifelse(upwind_from_search_monitors_12to13km > 0, "1",
                                                                                                                                ifelse(upwind_from_search_monitors_13to14km > 0, "1",
                                                                                                                                       ifelse(upwind_from_search_monitors_14to15km > 0, "1", "0")))))))))))))))) %>% 
  mutate(exposed = as.numeric(exposed))

## creating a column that depicts the total number of exposed days for each monitor site
results <- mutate(results, exposed_days = as.numeric(ifelse(exposed > 0, c(sum(results[8]),
                                                                           sum(results[9]),
                                                                           sum(results[10]),
                                                                           sum(results[11]),
                                                                           sum(results[12]),
                                                                           sum(results[13]),
                                                                           sum(results[14]),
                                                                           sum(results[15]),
                                                                           sum(results[16]),
                                                                           sum(results[17]),
                                                                           sum(results[18]),
                                                                           sum(results[19]),
                                                                           sum(results[20]),
                                                                           sum(results[21]),
                                                                           sum(results[22])), "0"))) 

saveRDS(results, file = "data/interim/Results_NO2_MDE_AQS_90.RDS")


## mapping Wheelabrator incinerator and monitor sites

# creating SF objects to determine distance
pollutant_sf <- pollutant_data %>%
  dplyr::select(site, Latitude, Longitude) %>% 
  st_as_sf(coords = c("Latitude", "Longitude"), crs = 4326)

## creating a dataframe using Wheelabrator lon + lat coordinates 
x = -76.629099
y = 39.270195
wheelabrator_address <- data.frame(x,y)
## converting wheelabrator data frame to sf object
wheelabrator_sf <- wheelabrator_address %>%
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  mutate(Site_ID = "wheelabrator incinerator")

## creating columns for the distance from the incinerator to each monitor a distance, & annuli
incin_distance <- st_distance(pollutant_sf, wheelabrator_sf) 
## joining results dataset to contain incin_distance file
pollutant_data <- cbind(pollutant_data, incin_distance) 

## loading in Baltimore shapefile and making it a polygon SF object
study_area <- readOGR("data/raw/shapefiles/BaltimoreCity.shp")
Bmore_sf = st_as_sf(study_area)
Bmore_sf_wgs = st_transform(Bmore_sf, st_crs(results_sf))
Bmore_polygon <- st_cast(Bmore_sf_wgs$geometry, "POLYGON")
Bmore_sf_lcc = st_transform(Bmore_sf, crs_projected)
Bmore <- st_cast(Bmore_sf_lcc$geometry, "POLYGON")

## plotting incinerator and monitor sites over Baltimore shapefile
plot(Bmore, col = 5, alpha=0.3)
plot(wheelabrator_sf_lcc,pch = 16, col = 7, alpha=0.4, add=T)
plot(results_sf_lcc, 
     pch = 16, cex = 1, col = 2, 
     add=T) 

## NEEDS WORK -- necessary to determine which monitor had the highest number of exposed days
## creating a new column shoeing the number of exposed days for each column

## creating a dataframe using Wheelabrator lon + lat coordinates 
x = -76.629099
y = 39.270195
wheelabrator_address <- data.frame(x,y)
## converting wheelabrator data frame to sf object
wheelabrator_sf <- wheelabrator_address %>%
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  mutate(site = "wheelabrator incinerator")
## projecting wheelabrator sf object in sae projection as narr data
wheelabrator_sf_lcc = st_transform(wheelabrator_sf, crs_projected)

## creating SF object for monitor sites associated with exposure assesment results
## unlisting the geometry column for latitude and longitude coordinates
results_sf <- results %>%
  mutate(lat = unlist(map(results$geometry,1)),
         long = unlist(map(results$geometry,2))) %>% 
  st_as_sf(coords = c("lat", "long"), crs = 4326)

results_sf_lcc = st_transform(results_sf, crs_projected)

## creating columns for the distance from the incinerator to each monitor a distance, & annuli
incin_distance <- st_distance(results_sf, wheelabrator_sf) 
## joining results dataset to contain incin_distance file
results <- cbind(results, incin_distance) 
## converting distance column to numeric for plotting preparation
results$incin_distance <- as.numeric(as.character(results$incin_distance))

## creating a vector of each monitor's annuli of exposure
annuli_field <- colnames(results[8:22])[apply(results[8:22],1,which.max)]
## creating a column that depicts each monitor's distance from the incin site
## creating an annuli column contains the original annuli column name of exposure for each monitor day
results$annuli_field = annuli_field
## creating an annuli column that depicts the annuli name (only) 
results$annuli = str_sub(results$annuli_field, 29, 36)


results <- results %>% 
  dplyr::select(-annuli_field)

## creating a variable that depicts thesum of the number of exposed days
## for each monitor-day

search_data_out <- results %>% 
  dplyr::select(-year, -month, -dow, -wind_direction, -geometry, -annuli_field)

## creating a box-grouped summary of results
exposed_days <- results %>% 
  dplyr::select(-date, -PM25_mgm3_mean, -Date, -wind_direction, -exposed_days, -exposed, -geometry) %>% 
  group_by(Site_ID, site) %>% 
  summarise_each(funs(sum))
## from here ^^ we can deduce which monitor had the max # of exposed days

## creating a box-grouped average of results
box_group_avg <- search_data_out %>% 
  dplyr::select(-date, -annuli) %>%
  ## subsetting out monitor-days of exposure
  subset(., exposed == 1) %>% 
  group_by(box_ID, site) %>% 
  summarise_each(funs(mean))
## from here ^^ we can deduce which monitor had the highest average PM concentrations

## calculating the mean pollutant concentration and SD for each annuli bin 
pm_raw_avg_annuli <- aggregate(pm25_raw ~ annuli, results, mean)
pm_raw_avg_annuli <- pm_raw_avg_annuli %>% 
  rename(pm25_raw_avg_annuli = pm25_raw)

pm_raw_avg_monitor <- aggregate(pm25_raw ~ box_ID, results, mean)
pm_raw_avg_monitor <- pm_raw_avg_monitor %>% 
  rename(pm_raw_avg_monitor = pm25_raw)

pm_raw_SD_annuli <- aggregate(pm25_raw ~ annuli, results, sd)
pm_raw_SD_annuli <- pm_raw_SD_annuli %>% 
  rename(pm25_raw_SD_annuli = pm25_raw)

pm_raw_SD_monitor <- aggregate(pm25_raw ~ box_ID, results, sd)
pm_raw_SD_monitor <- pm_raw_SD_monitor %>% 
  rename(pm_raw_SD_monitor = pm25_raw)

## adding the SD and mean values column, by annuli
results <- results %>%
  mutate(pm25_raw_avg_monitor_day = pm25_raw) %>%
  left_join(pm_raw_avg_annuli,by = c("annuli")) %>% 
  left_join(pm_raw_SD_annuli,by = c("annuli")) %>%
  left_join(pm_raw_avg_monitor,by = c("box_ID")) %>% 
  left_join(pm_raw_SD_monitor,by = c("box_ID"))

exposed_sum <- 
  c(sum(search_data_out[10]),
    sum(search_data_out[11]),
    sum(search_data_out[12]),
    sum(search_data_out[13]),
    sum(search_data_out[14]),
    sum(search_data_out[15]),
    sum(search_data_out[16]),
    sum(search_data_out[17]),
    sum(search_data_out[18]),
    sum(search_data_out[19]),
    sum(search_data_out[20]),
    sum(search_data_out[21]))

results <- mutate(results, exposed_days = case_when(
  exposed > 0 ~ sum(exposed_sum),
  TRUE ~ "0"))

search_data_out <- mutate(search_data_out, exposed_days = ifelse(exposed > 0, c(sum(search_data_out[10]),
                                                                                sum(search_data_out[11]),
                                                                                sum(search_data_out[12]),
                                                                                sum(search_data_out[13]),
                                                                                sum(search_data_out[14]),
                                                                                sum(search_data_out[15]),
                                                                                sum(search_data_out[16]),
                                                                                sum(search_data_out[17]),
                                                                                sum(search_data_out[18]),
                                                                                sum(search_data_out[19]),
                                                                                sum(search_data_out[20]),
                                                                                sum(search_data_out[21])), "0"))
## can't really remember what I was trying to accomplish here... 
results <- results %>% 
  as.numeric(as.character(results$exposed)) %>% 
  as.numeric(as.character(results$exposed_days))


## creating an empty data table to data population of the sum
## of observations of Wheelabrator exposure within each annuli
df <- data.frame(annuli = colnames(search_data_out[10:21]),
                 exposure_sum = c(sum((search_data_out[10])),
                                  sum((search_data_out[11])),
                                  sum((search_data_out[12])),
                                  sum((search_data_out[13])),
                                  sum((search_data_out[14])),
                                  sum((search_data_out[15])),
                                  sum((search_data_out[16])),
                                  sum((search_data_out[17])),
                                  sum((search_data_out[18])),
                                  sum((search_data_out[19])),
                                  sum((search_data_out[20])),
                                  sum((search_data_out[21]))), 
                 stringsAsFactors=FALSE) 