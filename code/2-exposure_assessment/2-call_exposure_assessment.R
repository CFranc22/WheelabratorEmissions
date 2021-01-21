## creating a dataframe using Wheelabrator lon + lat coordinates 

x = -76.629099
y = 39.270195

wheelabrator_address <- data.frame(x,y)
## converting wheelabrator data frame to sf object
wheelabrator_sf <- wheelabrator_address %>%
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  mutate(site = "wheelabrator incinerator")

####################################################################

## Loading in SEARCH pollutant data to create SF object
search_pollutants_daily <- readRDS("data/interim/search_daily_pm25") %>% 
  dplyr::select(site, box_ID, date, latitude,	longitude, pm25_raw, 
                year, month, dow)

## projecting SEARCH dataset to match NARR data
search_pollutants_sf <- search_pollutants_daily %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

####################################################################

narr_wind_direction <- readRDS("data/interim/narr_wind.rds") %>% 
  dplyr::select(box_ID, date, wind_direction) %>% 
  mutate(wind_direction = as.numeric(wind_direction))

## subsetting for dates with available pm 2.5 data
date1 <- as.Date("2018-11-01")
date2 <- as.Date("2019-11-18")

narr_wind_direction <- narr_wind_direction[narr_wind_direction$date >= date1 & narr_wind_direction$date<= date2,]

####################################################################

# creating input dataset
search_data_in <- search_pollutants_sf %>% 
  left_join(narr_wind_direction, by = c("box_ID", "date"))



nrow(search_data_in) # check length, then choose/edit options below as needed
#### e.g., if nrow(aqs_data_in) < 50000, change the second line as noted below

source("code/2-exposure_assessment/1-assess_exposure_wheelabrator_wind.R")

# initiates tibble to capture exposure data
search_daily_exp_annuli_wind <- list()

# loops through each birth, calls function to assesses exposure by trimester,
# and joins output to births dataset
for (i in c(1:nrow(search_data_in))) {  #### activate the appropriate line here
#for (i in c(1:10000)) {  #### activate the appropriate line here
#for (i in c(10001:25000)) {  
  #for (i in c(25001:50000)) {  
  #for (i in c(75001:nrow(aqs_data_in))) {  
  
  search_daily_exp_annuli_wind[[i]] <- 
    assessExposureAnnuliCountWind(search_data_in[i, ], 
                                  wheelabrator_sf,
                                   "upwind_from_search_monitors_")
  
  # prints the index value to track progress of the loop
  print(i)
  
}

search_data_out <- do.call("rbind", search_daily_exp_annuli_wind)

saveRDS(search_data_out,
        "data/processed/search_daily_annuli_wind_preproduction.rds")
