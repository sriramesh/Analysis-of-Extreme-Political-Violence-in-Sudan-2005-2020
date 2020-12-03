# make a gif
make_gif <- function(path, str) {
  
  ## list file names and read in
  imgs <- list.files(path, full.names = TRUE)
  img_list <- lapply(imgs, image_read)
  
  ## join the images together
  img_joined <- image_join(img_list)
  
  ## animate at 2 frames per second
  img_animated <- image_animate(img_joined, fps = 1)
  
  ## view animated image
  img_animated
  
  ## save to disk
  image_write(image = img_animated, path = paste0(path, str, '.gif'))
  
}

kde <- function(year_input) {

subset <- acled_sub %>% filter(year %in% year_input)

subset_spdf <- SpatialPointsDataFrame(
  coords = subset[,c("longitude", "latitude")],
  data = subset[,c("year", "event_type", "admin2", "fatalities")])

# define window
subset_win <- owin(xrange=range(subset$longitude),
                   yrange=range(subset$latitude))

# make ppp object
subset_battles_ppp <- ppp(subset$longitude, subset$latitude, window = subset_win, 
                          marks=as.factor(subset$i_extreme))

# plot bandwidths
subset_battles_ppp %>% density(bw.ppl) %>% plot(main = paste0("Hotspot Map of E.P.V., ", 
                                                              as.character(year_input)),
                                                col = heat.colors(10))

} # kernel density estimate
for(i in 2005:2011){
  jpeg(file=paste0("high-kde-graphs/kde_", as.character(i), ".jpeg"))
  kde(i)
  dev.off()
}
make_gif(path = 'high-kde-graphs/', str = 'high-kde')

for(i in 2012:2020){
  jpeg(file=paste0("low-kde-graphs/kde_", as.character(i), ".jpeg"))
  kde(i)
  dev.off()
}
make_gif(path = 'low-kde-graphs/', str = 'low-kde')

k_function <- function(year_input) {

subset <- acled_sub %>% filter(year %in% year_input)
subset_spdf <- SpatialPointsDataFrame(coords = subset[,c("longitude", "latitude")],
                                      data = subset[,c("year", "event_type", "admin2", "fatalities")])

# subset cases and controls into separate objects
cases <- subset_spdf[subset$i_extreme %in% 1, ]
controls <- subset_spdf[subset$i_extreme %in% 0, ]

# generate k function for CASES
cases_ppp <- cases %>% as("ppp")
# note different border-corrected estimates ('iso', 'border' and 'trans')
K <- cases_ppp %>% Kest(correction=c("isotropic", "Ripley")) # uses the "spatstat" package

# Plot the estimate of K(r) for CASES using MC simulation for the confidence interval/envelope
par(mfrow=c(1,1))
E <- cases_ppp %>% envelope(Kest, nsim=999)

E %>% plot(main=paste0("Monte Carlo Sim. of K-Function for E.P.V, ", as.character(year_input)),
           xlab="Distances (r)", lwd = 3,
           ylab="K-function K(r)")

} # Ripleys k function
for(i in 2005:2020){
  jpeg(file=paste0("k-function-graphs/kfunction_", as.character(i), ".jpeg"))
  k_function(i)
  dev.off()
}
make_gif(path = 'k-function-graphs/', str = 'k-function')

