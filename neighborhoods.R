#Load necessary packages
library(mdsr)
library(shiny)
library(tidyverse)
library(maps)
library(mapdata)


#Load the data
jan2016 <- read.csv('jan2016.csv')
centroids <- read.csv('NHoodNameCentroids.csv')

#clean centroid data
str(centroids)
centroids$longitude.latitude.section.borough <- as.character(centroids$longitude.latitude.section.borough)
str(centroids)

centroids <- centroids %>%
  separate(longitude.latitude.section.borough, into=c('n_longitude', 'n_latitude', 'neighborhood', 'borough'), sep=';') %>%
  mutate(n_longitude = as.numeric(n_longitude), n_latitude = as.numeric(n_latitude))

#create a file with the taxi pickup and dropoff locations
coordinates <- jan2016 %>%
  select(pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude)
write.csv(coordinates, 'cab_coordinates.csv')
  
cab_coordinates <- read.csv('cab_coordinates.csv') %>%
  select(-X) %>%
  filter(pickup_longitude != 0)

# Somehow join together the neighborhood data and the cab data?
# I want to compare the pickup longitude and latitude with each centroid longitude and latitude via the distance formula and then take the minimum distance


n <- 1:53525
m <- 1:299

test <- function(n,m) {
 (cab_coordinates$pickup_latitude[n] - centroids$n_latitude[m])^2 - (cab_coordinates$pickup_longitude[n] - centroids$n_longitude[m])^2
}

square_distance <- lapply(n, m, FUN=test)




