library("leaflet")
data <- read.csv(file = '../data/data_cleansed/mallorca/2020-09-19/listings.csv')
head(data)

df <- data.frame(name = data$id,
                 lat = data$latitude,
                 lng = data$longitude)
df %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions(),popup=data$price)