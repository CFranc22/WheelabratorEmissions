## creating final figures based on results processed and analyzed in the
## stats .R file

## reading in results datasets

plotting_data_unexposed <- readRDS("data/interim/Results_Analysis") %>% 
  ## subsetting out monitor-days of exposure
  subset(., exposed == 0) %>% 
  ## prepping dataset for plotting
  mutate(pm25_raw_avg_monitor_day = as.numeric(pm25_raw_avg_monitor_day)) %>% 
  mutate(pm25_raw_avg_annuli = as.numeric(pm25_raw_avg_annuli)) %>% 
  mutate(pm25_raw_SD_annuli = as.numeric(pm25_raw_SD_annuli)) %>% 
  mutate(pm_raw_avg_monitor = as.numeric(pm_raw_avg_monitor)) %>%
  mutate(pm_raw_SD_monitor = as.numeric(pm_raw_SD_monitor))

plotting_data_exposed <- readRDS("data/interim/Results_Analysis") %>% 
  ## subsetting out monitor-days of exposure
  subset(., exposed == 1) %>% 
  ## prepping dataset for plotting
  mutate(pm25_raw_avg_monitor_day = as.numeric(pm25_raw_avg_monitor_day)) %>% 
  mutate(pm25_raw_avg_annuli = as.numeric(pm25_raw_avg_annuli)) %>% 
  mutate(pm25_raw_SD_annuli = as.numeric(pm25_raw_SD_annuli)) %>% 
  mutate(pm_raw_avg_monitor = as.numeric(pm_raw_avg_monitor)) %>%
  mutate(pm_raw_SD_monitor = as.numeric(pm_raw_SD_monitor))

# creating a figure that depicts the effects of distance on PM 2.5 concentrations
# X-axis: annuli bins, y-axis: air pollutant concentrations

## creating a column that represents each exposed monitor's average's deviation from the overall PM 2.5 average
plotting_data_exposed <- mutate(plotting_data_exposed, pm_deviation_monitor_avg = as.numeric(plotting_data_exposed$pm_raw_avg_monitor - mean(plotting_data_exposed$pm25_raw)))
## creating a column that represents each annuli's average's deviation from the overall PM 2.5 average
plotting_data_exposed <- mutate(plotting_data_exposed, pm_deviation_annuli_avg = as.numeric(plotting_data_exposed$pm25_raw_avg_annuli - mean(plotting_data_exposed$pm25_raw)))

## creating the plot of distance vs. PM 2.5 concentrations
## plot of each exposed monitor's average PM concentration's deviation from average PM 2.5 concentrations
ggplot(plotting_data_exposed, aes(x=incin_distance, y=pm_deviation_monitor_avg, color=box_ID)) +
       geom_point(size=2, shape=16) +
       geom_errorbar(aes(ymin=(pm_deviation_monitor_avg-pm_raw_SD_monitor), ymax=(pm_deviation_monitor_avg+pm_raw_SD_annuli)), 
                width=.2,
                position=position_dodge(0.05))

# plot of each annuli's average PM concentration's deviation from average PM 2.5 values
ggplot(plotting_data_exposed, aes(x=incin_distance, y=pm_deviation_annuli_avg, color=box_ID)) +
  geom_point(size=2, shape=16) +
  geom_errorbar(aes(ymin=pm_deviation_annuli_avg-pm_raw_SD_annuli, ymax=pm_deviation_annuli_avg+pm_raw_SD_annuli),
                position=position_dodge(width=.2))

  
ggplot(plotting_data_exposed, aes(x=distance, y=pm25_raw_avg_annuli, fill=box_ID)) + 
  geom_line() +
  geom_point(stat="identity")+
  geom_errorbar(aes(ymin=pm25_raw_avg_annuli-pm25_raw_SD_annuli, ymax=pm25_raw_avg_annuli+pm25_raw_SD_annuli), 
                width=.2,
                position=position_dodge(0.05))

## reviewing David's notes
library("patchwork")

panel_g <- results_preprod_pm25 %>%
  filter(direction == "Upwind") %>%
  mutate(distance_ordinal = c(1:10)) %>%
  ggplot() + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(x     = distance_ordinal, 
                  y     = point_est, 
                  ymin  = ci_lower, 
                  ymax  = ci_upper),
              fill = "purple3", 
              alpha = 0.5) +
  geom_pointrange(aes(x     = distance_ordinal, 
                      y     = point_est, 
                      ymin  = ci_lower, 
                      ymax  = ci_upper),
                  color = "black") + 
  ylim(c(-3, 3.5)) +
  labs(y = "PM2.5 (µg/m^3)") +
  scale_x_discrete(name = "", 
                   limits = c("0-1", "1-2", "2-3", "3-4", "4-5",
                              "5-6", "6-7", "7-8", "8-9", "9-10")) +
  theme_classic() +
  theme(legend.position = "none") +
  ggtitle("g.")

## ideas for plotting
## wind intensity that leads to greatest increase in pollutant concentrations
## each monitor with their respective wind vector magnitude (blowing towards the monitor)
