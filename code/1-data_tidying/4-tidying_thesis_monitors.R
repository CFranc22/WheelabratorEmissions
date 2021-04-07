## tidying US EPA & MDE observational data 
## to characterize periods of function

## creating the US EPA AQS pollutant dataset using relevant columns created in 
## the revised excel file

NO2_EPA_AQS_2018 <- read.csv("data/interim/NO2_daily_2018.csv") %>%
  dplyr::select(Date, Site_ID, Daily_Max, UNITS, SITE_LATITUDE, SITE_LONGITUDE) %>% 
  rename(latitude = SITE_LATITUDE) %>% 
  rename(longitude = SITE_LONGITUDE) %>%
  rename(Units = UNITS)

NO2_EPA_AQS_2019 <- read.csv("data/interim/NO2_daily_2019.csv") %>%
  dplyr::select(Date, Site_ID, Daily_Max, UNITS, SITE_LATITUDE, SITE_LONGITUDE) %>% 
  rename(latitude = SITE_LATITUDE) %>% 
  rename(longitude = SITE_LONGITUDE) %>%
  rename(Units = UNITS)

NO2_EPA_AQS_2020 <- read.csv("data/interim/NO2_daily_2020.csv") %>%
  dplyr::select(Date, Site_ID, Daily_Max, UNITS, SITE_LATITUDE, SITE_LONGITUDE) %>% 
  rename(latitude = SITE_LATITUDE) %>% 
  rename(longitude = SITE_LONGITUDE) %>%
  rename(Units = UNITS)

# merging together all the air pollutant tibbles 2018-2020
NO2_EPA_AQS <- full_join(NO2_EPA_AQS_2018, NO2_EPA_AQS_2019) %>% 
  full_join(., NO2_EPA_AQS_2020) 

# converts date column to date class
NO2_EPA_AQS$Date <-
  NO2_EPA_AQS$Date %>%
  as.Date(format = "%m/%d/%y")  # forms date

saveRDS(NO2_EPA_AQS, file = "data/interim/NO2_EPA_AQS.RDS")

##########################################################################

## creating the MDE AQS pollutant dataset using relevant columns created in 
## the revised excel file

### loading in NOX data for tidying ###

NOX_MDE_AQS_0 <- read.csv("data/interim/NOX_Oldtown.csv") %>%
  dplyr::select(Site_ID, Date_LST, Value, Unit, Latitude, Longitude)
  
## creating a new dataframe with nox daily averages
nox_daily_avg_MDE_AQS <- aggregate(Value ~ Site_ID + Date_LST, NOX_MDE_AQS_0, mean)

## creating a dataframe based on nox  mean daily observations for each monitor-day
NOX_MDE_AQS <- NOX_MDE_AQS_0 %>% 
  dplyr::select(-Value) %>% 
  left_join(nox_daily_avg_MDE_AQS,by = c("Site_ID", "Date_LST")) %>% 
  mutate(NOX_PPB_mean = Value) %>% 
  dplyr::select(-Value)

NOX_MDE_AQS <- unique(NOX_MDE_AQS)

# converts date column to date class
NOX_MDE_AQS$Date_LST <-
  NOX_MDE_AQS$Date_LST %>%
  as.Date(format = "%m/%d/%y") # forms date

saveRDS(NOX_MDE_AQS, file = "data/interim/NOX_MDE_AQS.RDS")  

## loading in NO2 data for tidying ###

NO2_MDE_AQS_0 <- read.csv("data/interim/NO2_Oldtown.csv") %>%
  dplyr::select(Site_ID, Date_LST, Value, Unit, Latitude, Longitude)

## creating a new dataframe with no2 daily averages
no2_daily_avg_MDE_AQS <- aggregate(Value ~ Site_ID + Date_LST, NO2_MDE_AQS_0, mean)

## creating a dataframe based on nox  mean daily observations for each monitor-day
NO2_MDE_AQS <- NO2_MDE_AQS_0 %>% 
  dplyr::select(-Value) %>% 
  left_join(no2_daily_avg_MDE_AQS,by = c("Site_ID", "Date_LST")) %>% 
  mutate(NO2_PPB_mean = Value) %>% 
  dplyr::select(-Value)

NO2_MDE_AQS <- unique(NO2_MDE_AQS)

# converts date column to date class
NO2_MDE_AQS$Date_LST <-
  NO2_MDE_AQS$Date_LST %>%
  as.Date(format = "%m/%d/%y") # forms date

saveRDS(NO2_MDE_AQS, file = "data/interim/NO2_MDE_AQS.RDS")  

### loading in NO data for tidying ###

NO_MDE_AQS_0 <- read.csv("data/interim/NO_Oldtown.csv") %>%
  dplyr::select(Site_ID, Date_LST, Value, Unit, Latitude, Longitude)

## creating a new dataframe with no daily averages
no_daily_avg_MDE_AQS <- aggregate(Value ~ Site_ID + Date_LST, NO_MDE_AQS_0, mean)

## creating a dataframe based on nox  mean daily observations for each monitor-day
NO_MDE_AQS <- NO_MDE_AQS_0 %>% 
  dplyr::select(-Value) %>%  
  left_join(no_daily_avg_MDE_AQS,by = c("Site_ID", "Date_LST")) %>% 
  mutate(NO_PPB_mean = Value) %>% 
  dplyr::select(-Value)

NO_MDE_AQS <- unique(NO_MDE_AQS)

# converts date column to date class
NO_MDE_AQS$Date_LST <-
  NO_MDE_AQS$Date_LST %>%
  as.Date(format = "%m/%d/%y") # forms date

saveRDS(NO_MDE_AQS, file = "data/interim/NO_MDE_AQS.RDS")  

### loading in PM 2.5 data for tidying ###

PM25_MDE_AQS_0 <- read.csv("data/interim/PM25_Oldtown.csv") %>%
  dplyr::select(Site_ID, Date_LST, Value, Unit, Latitude, Longitude)

## creating a new dataframe with pm 2.5 daily averages
PM25_daily_avg_MDE_AQS <- aggregate(Value ~ Site_ID + Date_LST, PM25_MDE_AQS_0, mean)

## creating a dataframe based on nox  mean daily observations for each monitor-day
PM25_MDE_AQS <- PM25_MDE_AQS_0 %>% 
  dplyr::select(-Value) %>%  
  left_join(PM25_daily_avg_MDE_AQS,by = c("Site_ID", "Date_LST")) %>% 
  mutate(PM25_mgm3_mean = Value) %>% 
  dplyr::select(-Value)

PM25_MDE_AQS <- unique(PM25_MDE_AQS)

# converts date column to date class
PM25_MDE_AQS$Date_LST <-
  PM25_MDE_AQS$Date_LST %>%
  as.Date(format = "%m/%d/%y") # forms date

saveRDS(PM25_MDE_AQS, file = "data/interim/PM25_MDE_AQS.RDS")  

##########################################################################

## creating the SEARCH multipollutant wireless box dataset using 
## relevant columns created in the revised excel file

dat1b <- read.csv("data/interim/topCompressed_CombineSite_ABHI_All_PM25_Combined_EDT.csv") %>% 
  dplyr::select(-Box_ID)
dat1 <- dat1a %>% 
  left_join(dat1b,by = c("Box_ID"="Phy_Box", "Data_Name")) %>% 
  mutate(PM25 = as.numeric(values),
         date = substr(Date, 1, 9),
         date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  dplyr::select(-Object_ID, -values, -ind) 
  

pm25_daily_avg_SEARCH <- aggregate(PM25 ~ Data_Name + date, dat1, mean)

PM25_SEARCH <- dat1 %>% 
  dplyr::select(-PM25) %>% 
  left_join(pm25_daily_avg_SEARCH,by = c("date", "Data_Name")) %>% 
  mutate(PM25_mgm3_mean = PM25) %>% 
  dplyr::select(-PM25, -Date)

PM25_SEARCH <- unique(PM25_SEARCH)
PM25_SEARCH <- na.omit(PM25_SEARCH)

saveRDS(PM25_SEARCH, file = "data/interim/PM25_SEARCH_18-20.RDS")

