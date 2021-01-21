## creating a Fixed Effects Linear Regression Model (FELM)

## reading in covariates

search_pollutants_daily <- readRDS("data/interim/search_daily_pm25.RDS") %>% 
 mutate(monitor_day = as.factor(paste(box_ID, date, sep = "_"))) %>% 
  dplyr::select(-dow)

# results of exposure assessment + additional statistics
search_exposed <- readRDS("data/interim/Results_Analysis.RDS") %>% 
  mutate(monitor_day = as.factor(paste(box_ID, date, sep = "_"))) %>% 
  distinct(monitor_day, .keep_all = TRUE) %>% 
  dplyr::select(-pm25_raw, -year, -dow, -month, -geometry, -wind_direction)

narr_precip     <- readRDS("data/interim/narr_precip.rds") %>% 
  filter(date %in% search_exposed$date)
narr_temp       <- readRDS("data/interim/narr_temperature.rds") %>% 
  filter(date %in% search_exposed$date)
narr_rhum       <- readRDS("data/interim/narr_rhum.rds")
narr_pblh       <- readRDS("data/interim/narr_pblh.rds")

#.........................................................................

# make a dataset of monitor-days monitors > 15 km away the nearest new well
search_monitor_day_unexp <- search_pollutants_daily %>%
  as_tibble() %>%
  dplyr::select(box_ID, date) %>%
  # adds exposure variables and assigns 0 to all values
  mutate(upwind_from_search_monitors_0to1km   = 0,
         upwind_from_search_monitors_1to2km   = 0,
         upwind_from_search_monitors_2to3km   = 0,
         upwind_from_search_monitors_3to4km   = 0,
         upwind_from_search_monitors_4to5km   = 0,
         upwind_from_search_monitors_5to6km   = 0,
         upwind_from_search_monitors_6to7km   = 0,
         upwind_from_search_monitors_7to8km   = 0,
         upwind_from_search_monitors_8to9km   = 0,
         upwind_from_search_monitors_9to10km  = 0,
         upwind_from_search_monitors_10to11km = 0,
         upwind_from_search_monitors_11to12km = 0) %>% 
  mutate(monitor_day = as.factor(paste(box_ID, date, sep = "_"))) 

# makes the final analytic dataset
data_analytic <- search_exposed %>%
  # adds unexposed monitors data
  bind_rows(search_monitor_day_unexp) %>%
  mutate(monitor_day = as.factor(monitor_day)) %>%
  # adds NARR meteorological covariates
  left_join(narr_precip, by = c("box_ID", "date")) %>%
  left_join(narr_temp, by = c("box_ID", "date")) %>%
  left_join(narr_rhum, by = c("box_ID", "date")) %>%
  left_join(narr_pblh, by = c("box_ID", "date")) %>%
  # adds monitor-level covariates
  left_join(search_pollutants_daily, by = c("monitor_day", "site", "box_ID", "date")) %>% 
  mutate(box_ID = as.factor(box_ID))

data_analytic2 <- data_analytic %>%
  drop_na(pm25_raw) %>% 
  mutate(exposed = as.factor(exposed)) %>% 
  rename(annuli_0to1km = upwind_from_search_monitors_0to1km) %>% 
  rename(annuli_1to2km = upwind_from_search_monitors_1to2km) %>%
  rename(annuli_2to3km = upwind_from_search_monitors_2to3km) %>%
  rename(annuli_3to4km = upwind_from_search_monitors_3to4km) %>%
  rename(annuli_4to5km = upwind_from_search_monitors_4to5km) %>%
  rename(annuli_5to6km = upwind_from_search_monitors_5to6km) %>%
  rename(annuli_6to7km = upwind_from_search_monitors_6to7km) %>%
  rename(annuli_7to8km = upwind_from_search_monitors_7to8km) %>% 
  rename(annuli_8to9km = upwind_from_search_monitors_8to9km) %>% 
  rename(annuli_9to10km = upwind_from_search_monitors_9to10km) %>% 
  rename(annuli_10to11km = upwind_from_search_monitors_10to11km) %>% 
  rename(annuli_11to12km = upwind_from_search_monitors_11to12km)  
#drop_na(pm2.5_concentration_daily_mean)
#drop_na(o3_concentration_daily_max)



##------------------------------------------------------------------------------
## 

# visualize the distribution of 'pm2.5_concentration_daily_mean'
# make a histogram of month-years to see if we're well represented temporally
# see how many monitor-days we have within each air basin
# look at the distribution of precip
# explore the exposure data to get a better sense of how to factor it into our FELM


##------------------------------------------------------------------------------
## fits model

# model 1 - naive model, i.e., we don't control for anything, 
# a: the effect of annuli distance on PM concentrations
model_fit1a <- 
  felm(
    pm25_raw ~
      upwind_from_search_monitors_0to1km +
      upwind_from_search_monitors_1to2km + 
      upwind_from_search_monitors_2to3km +
      upwind_from_search_monitors_3to4km + 
      upwind_from_search_monitors_4to5km +
      upwind_from_search_monitors_5to6km +
      upwind_from_search_monitors_6to7km + 
      upwind_from_search_monitors_7to8km + 
      upwind_from_search_monitors_8to9km +
      upwind_from_search_monitors_9to10km,
    data = data_analytic2
  )
summary(model_fit1a)

# b: the effect of incin_distance on PM concentrations
model_fit1b <- 
  felm(
    pm25_raw ~
      incin_distance,
    data = data_analytic2
  )
summary(model_fit1b)

# model 2a - FE month + year + monitor, no meterological adjustments
# a: the effect of annuli distance on PM concentrations
model_fit2a <- 
  felm(
    pm25_raw ~
      upwind_from_search_monitors_0to1km +
      upwind_from_search_monitors_1to2km +
      upwind_from_search_monitors_2to3km +
      upwind_from_search_monitors_3to4km + 
      upwind_from_search_monitors_4to5km + 
      upwind_from_search_monitors_5to6km +
      upwind_from_search_monitors_6to7km + 
      upwind_from_search_monitors_7to8km + 
      upwind_from_search_monitors_8to9km +
      upwind_from_search_monitors_9to10km 
    | as.factor(year)+
      as.factor(month)+
      box_ID,
    data = data_analytic2
  )
summary(model_fit2a)

# b: the effect of incin_distance on PM concentrations
model_fit2b <- 
  felm(
    pm25_raw ~
      incin_distance
    | as.factor(year)+
      as.factor(month)+
      box_ID,
    data = data_analytic2
  )
summary(model_fit2b)

# model 3 - FE monitor + month + year, adj. for precip + rhum + temp + PBLH
# a: the effect of annuli distance on PM concentrations
model_fit3a <- 
  lfe::felm(
    pm25_raw ~
      annuli_0to1km +
      annuli_1to2km +
      annuli_2to3km + 
      annuli_3to4km + 
      annuli_4to5km + 
      annuli_5to6km +
      annuli_6to7km + 
      annuli_7to8km + 
      annuli_8to9km +
      annuli_9to10km +
      annuli_10to11km +
      annuli_11to12km +
      exposed +
      precip +
      rhum +
      pblh +
      temperature
    |as.factor(year)+
      as.factor(month) +
      box_ID,
    data = data_analytic2
  )
summary(model_fit3a)

# b: the effect of incin_distance on PM concentrations
model_fit3b <- 
  lfe::felm(
    pm25_raw ~
      incin_distance +
      exposed +
      precip +
      rhum +
      pblh + 
      temperature
    |as.factor(year)+
      as.factor(month) +
      as.factor(box_ID),
    data = data_analytic2
  )
summary(model_fit3b)

# still have not worked out bugs when it comes to using the tidy function, and 
# all subseqquent code below

model3_results <- tidy(model_fit3a, conf.int = TRUE, conf.level = 0.95, fe=TRUE)

broom.tidy(model_fit3a)

  replace_na(list(estimate = 0, conf.low = 0, conf.high = 0)) %>%
  mutate(term = as.factor(term)) %>%
  #mutate(Distance = case_when(term == "wells_new_0to1" ~ "0-1")) %>%
  filter(term != "narr_precip") %>% 
  #mutate(Distance = as.factor(Distance)) %>%
  as_tibble()
model3_results

model3_results %>%
  ggplot() +
  geom_pointrange(aes(x    = term, 
                      y    = estimate,
                      ymin = conf.low,
                      max  = conf.high)) +
  labs(x = "Distance (km)", 
       y = "Marginal change in PM2.5 concentration (µg/m^3)") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()


##============================================================================##
