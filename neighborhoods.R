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
  select(-X)

# Somehow join together the neighborhood data and the cab data?
# I want to compare the pickup longitude and latitude with each centroid longitude and latitude via the distance formula and then take the minimum distance


n <- 1:nrow(cab_coordinates)
m <- 1:nrow(centroids)

pickup_distance <- function(m,n) {
  sqrt((cab_coordinates$pickup_latitude[n] - centroids$n_latitude[m])^2 + (cab_coordinates$pickup_longitude[n] - centroids$n_longitude[m])^2)
}

distance <- lapply(m,n, FUN=pickup_distance)
distance <- as.data.frame(distance)
colnames(distance) = centroids[,3]
pickup_nhood <- colnames(distance)[apply(distance, 1, which.min)]
pickup_nhood <- as.data.frame(pickup_nhood)

dropoff_distance <- function(m,n) {
  sqrt((cab_coordinates$dropoff_latitude[n] - centroids$n_latitude[m])^2 + (cab_coordinates$dropoff_longitude[n] - centroids$n_longitude[m])^2)
}

distance1 <- lapply(m,n, FUN=dropoff_distance)
distance1 <- as.data.frame(distance1)
colnames(distance1) = centroids[,3]
dropoff_nhood <- colnames(distance1)[apply(distance1, 1, which.min)]
dropoff_nhood <- as.data.frame(dropoff_nhood)


# Bind the two neighborhoods (pickup and dropoff) into one data frame
neighborhoods <- cbind(pickup_nhood, dropoff_nhood)
jan2016 <- cbind(jan2016, neighborhoods)


