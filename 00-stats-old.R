# convert to spdf
acled_spdf <- SpatialPointsDataFrame(
  coords = acled[,c("longitude", "latitude")],
  data = acled[,c("year", "event_type", "sub_event_type", "actor1",
                  "assoc_actor_1", "region", "country", "admin1",
                  "admin2", "admin3", "location", "source", "fatalities")])

# define window
acled_win <- owin(xrange=range(acled$longitude),
                 yrange=range(acled$latitude))

# make ppp object
acled_battles_ppp <- ppp(acled$longitude, acled$latitude, window = acled_win, 
                         marks=as.factor(acled$i_battles))

# plot bandwidths
acled_battles_ppp %>% density(bw.ppl) %>% plot(main = "CV-based bandwidth selection",
                                               col = heat.colors(10))

# relative risk?
acled_battles_ppp <- ppp(acled$longitude, acled$latitude, window = acled_win, 
                         marks=as.factor(acled$i_battles))

rel_risk_est <- acled_battles_ppp %>% relrisk(relative = T)

par(mfrow=c(1,1))
rel_risk_est %>% plot(main="Relative Risk Estimate")

# ripley's k ----

# subset cases and controls into separate objects
cases <- acled_spdf[acled$i_battles %in% 1, ]
controls <- acled_spdf[acled$i_battles %in% 0, ]

# generate k function for CASES
cases_ppp <- cases %>% as("ppp")
# note different border-corrected estimates ('iso', 'border' and 'trans')
K <- cases_ppp %>% Kest(correction=c("isotropic", "Ripley")) # uses the "spatstat" package

# Plot the estimate of K(r) for CASES using MC simuluation for the confidence interval/envelope
par(mfrow=c(1,1))
E <- cases_ppp %>% envelope(Kest, nsim=999)
E %>% plot(main="Monte Carlo simluation (nsim=999) of the K-function for Cases only",
           xlab="Distances (r)",
           ylab="K-function K(r)")
