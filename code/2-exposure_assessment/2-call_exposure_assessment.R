## creating a dataframe using Wheelabrator lon + lat coordinates 

x = -76.629099
y = 39.270195

wheelabrator_address <- data.frame(x,y)
## converting wheelabrator data frame to sf object
wheelabrator_sf <- wheelabrator_address %>%
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  mutate(site = "wheelabrator incinerator")

####################################################################

## Loading in US EPA AQS pollutant data to create a SF object
NO2_EPA_AQS <- readRDS("data/interim/NO2_EPA_AQS.RDS") %>% 
  dplyr::select(Site_ID, Date, latitude,	longitude, Daily_Max)

## Loading in MDE AQS pollutant datasets to create respective SF objects
NOX_MDE_AQS <- readRDS("data/interim/NOX_MDE_AQS.RDS") %>% 
  dplyr::select(Site_ID, Date_LST, Latitude,	Longitude, NOX_PPB_mean)

NO_MDE_AQS <- readRDS("data/interim/NO_MDE_AQS.RDS") %>% 
  dplyr::select(Site_ID, Date_LST, Latitude,	Longitude, NO_PPB_mean)

NO2_MDE_AQS <- readRDS("data/interim/NO2_MDE_AQS.RDS") %>% 
  dplyr::select(Site_ID, Date_LST, Latitude,	Longitude, NO2_PPB_mean)

PM25_MDE_AQS <- readRDS("data/interim/PM25_MDE_AQS.RDS") %>% 
  dplyr::select(Site_ID, Date_LST, Latitude,	Longitude, PM25_mgm3_mean)

 ## Loading in SEARCH multipollutant wireless dataset to create an SF object

PM25_SEARCH <- readRDS("data/interim/PM25_SEARCH.RDS") %>% 
  dplyr::select(site, date, latitude,	longitude, PM25_mgm3_mean) %>% 
  mutate(Date = as.Date(date)) %>% 
  mutate(Site_ID = site) 

####################################################################

narr_wind_direction <- readRDS("data/interim/narr_wind.rds") %>% 
  dplyr::select(box_ID, date, wind_direction) %>% 
  mutate(wind_direction = as.numeric(wind_direction)) %>% 
  mutate(Site_ID = box_ID) %>% 
  mutate(Date = date) %>% 
  dplyr::select(-box_ID, -date)

# converts date column to date class
narr_wind_direction$Date <-
  narr_wind_direction$Date %>%
  as.Date(format = "%Y-%m-%d")  # forms date

## subsetting for dates with available pollutant data
date1 <- as.Date("2018-11-01")
date2 <- as.Date("2019-11-18")

narr_wind_direction <- narr_wind_direction[narr_wind_direction$Date >= date1 & narr_wind_direction$Date<= date2,] %>% 
  mutate(Site_ID = as.character(Site_ID)) 
#NO2_EPA_AQS <- NO2_EPA_AQS[NO2_EPA_AQS$Date >= date1 & NO2_EPA_AQS$Date<= date2,]
#PM25_MDE_AQS <- PM25_MDE_AQS[PM25_MDE_AQS$Date >= date1 & PM25_MDE_AQS$Date<= date2,] %>% mutate(Date = as.Date(Date_LST))
PM25_SEARCH <- PM25_SEARCH[PM25_SEARCH$Date >= date1 & PM25_SEARCH$Date<= date2,]
 


## projecting pollutant dataset to match NARR data
#NO2_EPA_AQS_sf <- NO2_EPA_AQS %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

PM25_SEARCH_sf <- PM25_SEARCH %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

#PM25_SEARCH_sf <- PM25_SEARCH %>%st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

####################################################################

# creating input dataset
data_in <- PM25_SEARCH_sf %>% 
  left_join(narr_wind_direction, by = c("Site_ID", "Date"))

data_in <- na.omit(data_in)

nrow(data_in) # check length, then choose/edit options below as needed
#### e.g., if nrow(aqs_data_in) < 50000, change the second line as noted below

source("code/2-exposure_assessment/1-assess_exposure_wheelabrator_wind.R")

# initiates tibble to capture exposure data
temporary_stopgap_dataframe <- list()


# loops through each birth, calls function to assesses exposure by trimester,
# and joins output to births dataset
for (i in c(1:nrow(data_in))) {  #### activate the appropriate line here
#for (i in c(1:10000)) {  #### activate the appropriate line here
#for (i in c(10001:25000)) {  
  #for (i in c(25001:50000)) {  
  #for (i in c(75001:nrow(aqs_data_in))) {  
  
  temporary_stopgap_dataframe[[i]] <- 
    assessExposureAnnuliCountWind(data_in[i, ], 
                                  wheelabrator_sf,
                                  # input desired angle for exposure wedge, keep ""s
                                  "30", 
                                   "upwind_from_search_monitors_")
  
  # prints the index value to track progress of the loop
  print(i)
  
}

data_out <- do.call("rbind", temporary_stopgap_dataframe)

saveRDS(data_out,
        "data/processed/PM25_SEARCH_daily_mean_30.rds")

