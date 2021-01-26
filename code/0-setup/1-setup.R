# 0.1 - attaches necessary packages and defines global variables

# loads necessary packages

    library(dplyr)
    library(raster)
    library(sf)
    library(rgdal)
    library(tidyr)
    library(tidyverse)
    library(ncdf4)
    library(lubridate)
    library(lfe)
    library(broom)
  
    # defines global variables

    '%!in%' <- function(x,y)!('%in%'(x,y))
    
  # coordinate reference system (CRS) for the project
    # unprojected CRS, NAD83, for geographic data
    crs_nad83 <- st_crs("+init=epsg:4269 +proj=longlat +ellps=GRS80
                        +datum=NAD83 +no_defs +towgs84=0,0,0")  
    
    # projected CRS, for creating buffers
    crs_projected <- st_crs(stack("data/raw/narr/air_temp_daily_mean/air.sfc.2018.nc"))
   
# end #