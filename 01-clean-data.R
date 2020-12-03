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

