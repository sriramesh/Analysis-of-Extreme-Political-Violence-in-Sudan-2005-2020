# visualization
library(ggplot2)
library(spatstat)
library(plotrix)
library(fields)
library(leaflet)
library(maptools)
library(RColorBrewer)
library(lattice)
library(geoR)
library(plotrix) 
library(RcppArmadillo)

# spatial data management and point process analysis
library(sp)
library(gtools)

# point processes
library(spatstat)
library(splancs) # K-function
library(smacpod) # Spatial scanning statistic
library(car) # contains a function for logistic transformation (log odds ratio) to make more normal

library(leaflet)
library(tidyverse)
library(shiny)
library(xts)
library(knitr)

# read in csv
acled <- read.csv("1900-01-01-2020-05-06-Sudan.csv", sep = ";") # Sudan

# select for year 2005+ (after CPA was signed), filter, and mutate 
acled_sub <- acled %>% select(year, event_type, admin2, fatalities, longitude, latitude) %>%
  filter(year >= 2005) %>%
  mutate(i_extreme = ifelse(event_type %in% "Battles" | 
                              event_type %in% "Explosions/Remote violence" | 
                              event_type %in% "Violence against civilians",
                            1, 0),
         i_extreme_label = ifelse(event_type %in% "Battles" | 
                                    event_type %in% "Explosions/Remote violence" | 
                                    event_type %in% "Violence against civilians",
                                  'Extreme violence', 'Non-extreme violence'))


# ui
fluidPage(
  titlePanel("Mapping Violence in Post-War Sudan"),
  sliderInput("year",
              label = "Years 2005 to 2020",
              min = min(acled_sub$year), 
              max = max(acled_sub$year),
              value = max(acled_sub$year),
              step=1,
              animate = animationOptions(interval = 1400)),
  leafletOutput("mymap")
)