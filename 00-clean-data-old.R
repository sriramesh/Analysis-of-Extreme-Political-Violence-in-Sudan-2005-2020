options(stringsAsFactors = F)

#acled <- read.csv("1900-01-01-2020-05-06-Sri_Lanka.csv", sep = ";") #Sri Lanka

labs <- lapply(seq(nrow(acled)), function(i) {
  paste0( '<p>', "Source: ", acled[i, "source"], '<p>', 
          "Event Type: ", acled[i, "event_type"], '</p>', 
          "Event Date: ", acled[i, "event_date"], '</p>', 
          "Fatalities: ", acled[i, "fatalities"], '</p>' ) 
})

pal = colorFactor("Dark2", acled$event_type)

acled_map <- acled %>%
  leaflet() %>% addTiles(options = tileOptions(opacity = .6)) %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   fillOpacity=1,
                   fillColor = ~pal(event_type),
                   radius=~fatalities*.03,
                   weight=0.1,
                   stroke=TRUE,
                   label = lapply(labs, htmltools::HTML)) %>%
  addLegend(pal = pal, values = ~event_type, title = "Event Type")

acled_map