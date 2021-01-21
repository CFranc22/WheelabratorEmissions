# getting to know the results of the exposure assessment 

results <- readRDS("data/processed/search_daily_annuli_wind_preproduction.rds")

## reading in necessary datasets
## creating a dataframe using Wheelabrator lon + lat coordinates 

x = -76.629099
y = 39.270195

wheelabrator_address <- data.frame(x,y)
## converting wheelabrator data frame to sf object
wheelabrator_sf <- wheelabrator_address %>%
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  mutate(site = "wheelabrator incinerator")

## creating SEARCH pm 2.5 dataset using relevant columns created in 
##the revised excel file

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

saveRDS(search_pollutants_daily, file = "data/interim/search_daily_pm25")

## projecting SEARCH dataset to match NARR data
## creating the SEARCH monitor-day pollutant 
##SF object, datum/epsg = WGS 84 in the same projection as the narr datasets

search_pollutants_sf <- search_pollutants_daily %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

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
                                                                                                                                ifelse(upwind_from_search_monitors_11to12km > 0, "1","0"))))))))))))) %>% 
  mutate(exposed = as.numeric(exposed))
## creating a dataset that encompasses the distance between each monitor site
## and the wheelabrator incinerator site

incin_distance <- st_distance(search_pollutants_sf, wheelabrator_sf) 

## joining results dataset to contain incin_distance file
results <- cbind(results, incin_distance) 
## converting distance column to numeric for plotting preparation
results$incin_distance <- as.numeric(as.character(results$incin_distance))

## creating a vector of each monitor's annuli of exposure
annuli_field <- colnames(results[10:21])[apply(results[10:21],1,which.max)]

## creating a column that depicts each monitor's distance from the incin site
## creating an annuli column contains the original annuli column name of exposure for each monitor day
results$annuli_field = annuli_field
## creating an annuli column that depicts the annuli name (only) 
results$annuli = str_sub(results$annuli_field, 29, 36)

## calculating the mean and SD for each annuli bin 
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

saveRDS(results, file = "data/interim/Results_Analysis.RDS")

## i need to create a code that counts the number of 

## NEEDS WORK -- necessary to determine which monitor had the highest number of exposed days
## creating a new column shoeing the number of exposed days for each column

## creating a variable that depicts thesum of the number of exposed days
## for each monitor-day

search_data_out <- results %>% 
  dplyr::select(-year, -month, -dow, -wind_direction, -geometry, -annuli_field)

## creating a box-grouped summary of results
box_group_sum <- search_data_out %>% 
  dplyr::select(-date, -annuli) %>% 
  group_by(box_ID, site) %>% 
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
