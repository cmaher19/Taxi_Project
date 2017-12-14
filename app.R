# If you just click the Run App button, this should work.

# Load necessary packages
library(mdsr)
library(shiny)
library(broom)
library(tidyverse)
library(rgdal)
library(shinythemes)
library(lubridate)
library(leaflet)
library(ggmap)
library(googleway)


# Load the data
jan2016 <- read.csv('jan1.csv')
feb2016 <- read.csv('feb1.csv')
mar2016 <- read.csv('mar1.csv')
apr2016 <- read.csv('apr1.csv')
may2016 <- read.csv('may1.csv')
jun2016 <- read.csv('jun1.csv')
cab2016 <- rbind(jan2016, feb2016, mar2016, apr2016, may2016, jun2016)
centroids <- read.csv('NHoodNameCentroids.csv')

# DATA WRANGLING

# Data Wrangling Part 1: Get information about neighborhood pickup and dropoff onto cab data

# Clean centroid data
centroids <- centroids %>%
  mutate(longitude.latitude.section.borough = as.character(longitude.latitude.section.borough)) %>%
  separate(longitude.latitude.section.borough, into=c('n_longitude', 'n_latitude', 'neighborhood', 'borough'), sep=';') %>%
  mutate(n_longitude = as.numeric(n_longitude), n_latitude = as.numeric(n_latitude))

# There are four neighborhoods names that exist in two places
# This is causing issues especially for the 'Chelsea' neighborhoods
# because the one in Manhattan is oftentimes getting encoded as being in 
# Staten Island
repeat_neighborhoods <- centroids %>%
  group_by(neighborhood) %>%
  summarise(n=n()) %>%
  filter(n>1)

# For Chelsea and Murray Hill, I kept the Manhattan locations
# For Sunnyside and Bay Terrace, I kept the Queens locations
centroids <- centroids[-247,]
centroids <- centroids[-238,]
centroids <- centroids[-183,]
centroids <- centroids[-223,]


# Create a file with the taxi pickup and dropoff locations
coordinates <- cab2016 %>%
  select(pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude)
write.csv(coordinates, 'cab_coordinates.csv')

cab_coordinates <- read.csv('cab_coordinates.csv') %>%
  select(-X)

# Find nearest neighborhood for pickup based on coordinates
n <- 1:nrow(cab_coordinates)
m <- 1:nrow(centroids)

pickup_distance <- function(m,n) {
  sqrt((cab_coordinates$pickup_latitude[n] - centroids$n_latitude[m])^2 + (cab_coordinates$pickup_longitude[n] - centroids$n_longitude[m])^2)
}

start_distance <- lapply(m,n, FUN=pickup_distance)
start_distance <- data.frame(start_distance)
colnames(start_distance) = centroids[,3]
pickup_nhood <- colnames(start_distance)[apply(start_distance, 1, which.min)]
pickup_nhood <- data.frame(pickup_nhood)

# Find nearest neighborhood for dropoff based on coordinates
dropoff_distance <- function(m,n) {
  sqrt((cab_coordinates$dropoff_latitude[n] - centroids$n_latitude[m])^2 + (cab_coordinates$dropoff_longitude[n] - centroids$n_longitude[m])^2)
}

end_distance <- lapply(m,n, FUN=dropoff_distance)
end_distance <- data.frame(end_distance)
colnames(end_distance) = centroids[,3]
dropoff_nhood <- colnames(end_distance)[apply(end_distance, 1, which.min)]
dropoff_nhood <- data.frame(dropoff_nhood)

# Bind the two neighborhoods (pickup and dropoff) and add to the cab dataset
neighborhoods <- cbind(pickup_nhood, dropoff_nhood)
cab2016 <- cbind(cab2016, neighborhoods)

# Join neighborhood centroid data onto yellow cab data for both pickup and dropoff location
# We need neighborhood lat and long in later mapping
# The warning message here is fine - it did some of the work for us by making the neighborhood
# variables in the cab dataset into characters
cab2016 <- left_join(cab2016, centroids, by=c('pickup_nhood'='neighborhood')) %>%
  rename(pickup_borough = borough, pickup_zone=pickup_nhood)

cab2016 <- left_join(cab2016, centroids, by=c('dropoff_nhood'='neighborhood')) %>%
  rename(dropoff_borough = borough, dropoff_zone=dropoff_nhood)

# Data Wrangling Part Two: Clean up cab2016 dataset 

# Convert pickup and dropoff date and time information into date-time objects
cab2016 <- cab2016 %>%
  mutate(tpep_pickup_datetime = as.POSIXct(tpep_pickup_datetime),
         tpep_dropoff_datetime = as.POSIXct(tpep_dropoff_datetime)) 

# Create a total trip duration variable (in minutes)
cab2016 <- cab2016 %>%
  mutate(trip_duration = difftime(tpep_dropoff_datetime, tpep_pickup_datetime, unit='min'))

# Split the data and time aspects into two separate variables
# Trim extra space from the front of the time variables
# Convert pickup date into actual date variable
cab2016 <- cab2016 %>%
  separate(tpep_pickup_datetime, into=c('pickup_date', 'pickup_time'), sep=10) %>%
  separate(tpep_dropoff_datetime, into=c('dropoff_date', 'dropoff_time'), sep=10) %>%
  mutate(pickup_time = trimws(pickup_time, which='left'),
         dropoff_time = trimws(dropoff_time, which='left'))

# Separate the time variables into hours and minutes and seconds, then factor them
# After going through all of our analyses, we never used minutes or seconds so I'm going
# to remove them here
cab2016 <- cab2016 %>%
  separate(pickup_time, into = c('pickup_hour', 'pickup_minute', 'pickup_seconds'), sep=':') %>%
  separate(dropoff_time, into = c('dropoff_hour', 'dropoff_minute', 'dropoff_seconds'), sep=':') %>%
  mutate(pickup_hour = as.factor(pickup_hour), dropoff_hour = as.factor(dropoff_hour)) %>%
  select(-pickup_minute, -pickup_seconds, -dropoff_minute, -dropoff_seconds)

# Convert pickup and dropoff dates to actual date variables so that weekday can be extracted
cab2016 <- cab2016 %>%
  mutate(pickup_date = as.Date(pickup_date), dropoff_date = as.Date(dropoff_date)) %>%
  mutate(weekday = weekdays(pickup_date))

# Separate date variables into year, month, and day separately so that they are easier to 
# match with user input
# Convert to factors for model building
cab2016 <- cab2016 %>%
  separate(pickup_date, into=c('pickup_year', 'pickup_month', 'pickup_day'), sep='-') %>%
  separate(dropoff_date, into=c('dropoff_year', 'dropoff_month', 'dropoff_day'), sep='-') %>%
  mutate(pickup_month = as.factor(pickup_month), pickup_day = as.factor(pickup_day),
         dropoff_month = as.factor(dropoff_month), dropoff_day = as.factor(dropoff_day))

# Filter out observations with pickup/dropoff coordinates with zeroes and rates other 
# than the standard fare rates (there were some shady exchanges in the nonstandard fare options)
# Also removing variables that we didn't use
# All of the payment variables were summed in the total amount, so we removed individual charges
cab2016 <- cab2016 %>%
  filter(RatecodeID == 1, pickup_longitude != 0, dropoff_longitude != 0) %>%
  select(-VendorID, -store_and_fwd_flag, -payment_type, -fare_amount, -extra, -mta_tax, -tip_amount, 
         -tolls_amount, -improvement_surcharge)

# UNIVARIATE ANALYSES

# Look at and do some filtering on total amount ($ spent on cab ride)
cab2016 %>%
  filter(total_amount < 60) %>%
  ggplot(aes(x=total_amount)) + geom_density(fill='purple', alpha=0.2) + xlab('Total Amount of Fare ($)') + ylab('Density') +
  ggtitle('Density of Total $ Spent on Cab Ride')

cab2016 %>%
  filter(total_amount > 60) %>%
  summarise(n=n()) # only 122 rides that cost more than 75 dollars, we can do without them

cab2016 <- cab2016 %>%
  filter(total_amount < 60 & total_amount > 0)

# Look at and do some filtering on trip duration
cab2016 %>%
  filter(trip_duration < 75) %>%
  ggplot(aes(x=trip_duration)) + geom_density(fill='purple', alpha=0.2) + xlab('Trip Duration (minutes)') + ylab('Density') +
  ggtitle('Density of Trip Durations (in minutes)')

cab2016 %>%
  filter(trip_duration > 75) %>%
  summarise(n=n()) # only 87 rides that were longer than 75 minutes, we can do without them

cab2016 <- cab2016 %>%
  filter(trip_duration < 75)

# Look at and filter trip distance

# Some of these are literally not possible with my knowledge of New York
# For example, the distance between Midtown and Chelsea could not possibly be zero
# We are going to filter out trips with distances of zero that somehow went
# from one neighborhood to another
cab2016 %>%
  filter(trip_distance==0 & pickup_zone != dropoff_zone)

cab2016 %>%
  filter(trip_distance > 20) #I'm only going to remove the two of these that are definitely wrong, I'll filter out trips >27mi

cab2016 <- cab2016 %>%
  filter(trip_distance != 0 & trip_distance < 27)


# MODEL BUILDING 
# End goal: Take user input and use it to build predictive models for fare prediction and time duration

# MODEL BUILDING PART ONE: trip distance (this wasn't one of the two model we said we'd build but it is useful to 
# use in conjunction with the other models)

# Graphs broken down by borough to make it more readable
# There are clear differences between zones --> use in model
cab2016 %>%
  filter(pickup_borough == "Bronx") %>%
  ggplot(aes(x=pickup_zone, y=trip_distance, col=pickup_zone)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab('Pickup Zone') + 
  ylab('Trip Distance (mi)') + ggtitle('Trip Distance by Pickup Zone in the Bronx') + labs(color = 'Pickup Zone')

cab2016 %>%
  filter(pickup_borough == "Brooklyn") %>%
  ggplot(aes(x=pickup_zone, y=trip_distance, col=pickup_zone)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab('Pickup Zone') + 
  ylab('Trip Distance (mi)') + ggtitle('Trip Distance by Pickup Zone in Brooklyn') + labs(color = 'Pickup Zone')

cab2016 %>%
  filter(pickup_borough == "Manhattan") %>%
  ggplot(aes(x=pickup_zone, y=trip_distance, col=pickup_zone)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab('Pickup Zone') + 
  ylab('Trip Distance (mi)') + ggtitle('Trip Distance by Pickup Zone in Manhattan') + labs(color = 'Pickup Zone')

cab2016 %>%
  filter(pickup_borough == "Queens") %>%
  ggplot(aes(x=pickup_zone, y=trip_distance, col=pickup_zone)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab('Pickup Zone') + 
  ylab('Trip Distance (mi)') + ggtitle('Trip Distance by Pickup Zone in Queens') + labs(color = 'Pickup Zone')

cab2016 %>%
  filter(pickup_borough == "Staten Island") %>%
  ggplot(aes(x=pickup_zone, y=trip_distance, col=pickup_zone)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab('Pickup Zone') + 
  ylab('Trip Distance (mi)') + ggtitle('Trip Distance by Pickup Zone in Staten Island') + labs(color = 'Pickup Zone')

# Same graphs but for dropoff zone instead of pickup zone
# You have to zoom these graphs to see them in full
cab2016 %>%
  filter(dropoff_borough == "Bronx") %>%
  ggplot(aes(x=dropoff_zone, y=trip_distance, col=dropoff_zone)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab('Dropoff Zone') + 
  ylab('Trip Distance (mi)') + ggtitle('Trip Distance by Dropoff Zone in the Bronx') + labs(color = 'Dropoff Zone')

cab2016 %>%
  filter(dropoff_borough == "Brooklyn") %>%
  ggplot(aes(x=dropoff_zone, y=trip_distance, col=dropoff_zone)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab('Dropoff Zone') + 
  ylab('Trip Distance (mi)') + ggtitle('Trip Distance by Dropoff Zone in Brooklyn') + labs(color = 'Dropoff Zone')

cab2016 %>%
  filter(dropoff_borough == "Manhattan") %>%
  ggplot(aes(x=dropoff_zone, y=trip_distance, col=dropoff_zone)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab('Dropoff Zone') + 
  ylab('Trip Distance (mi)') + ggtitle('Trip Distance by Dropoff Zone in Manhattan') + labs(color = 'Dropoff Zone')

cab2016 %>%
  filter(dropoff_borough == "Queens") %>%
  ggplot(aes(x=dropoff_zone, y=trip_distance, col=dropoff_zone)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab('Dropoff Zone') + 
  ylab('Trip Distance (mi)') + ggtitle('Trip Distance by Dropoff Zone in Queens') + labs(color = 'Dropoff Zone')

cab2016 %>%
  filter(dropoff_borough == "Staten Island") %>%
  ggplot(aes(x=dropoff_zone, y=trip_distance, col=dropoff_zone)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab('Dropoff Zone') + 
  ylab('Trip Distance (mi)') + ggtitle('Trip Distance by Dropoff Zone in Staten Island') + 
  labs(color = 'Dropoff Zone')

# Create model
# We tried models with other things but they never helped the performance of the model
# The models R squared value is okay but the diagnostic plots look horrible
# There is definitely a better way to do this but we didn't find it
distance_mod <- lm(trip_distance ~ pickup_zone + dropoff_zone + pickup_borough
                   + dropoff_borough, data=cab2016)
summary(distance_mod)
plot(which=1, distance_mod)
plot(which=2, distance_mod)
plot(which=4, distance_mod)
plot(which=5, distance_mod)

cab2016 <- cab2016 %>%
  mutate(distance_prediction = predict(distance_mod, cab2016))

# MODEL BUILDING PART TWO: Total fare amount ($)

# We had trouble with these high outliers with every variable
# There is a bit of variation (a few dollars) between hours so this may be useful
cab2016 %>%
  ggplot(aes(x=pickup_hour, y=total_amount, col=pickup_hour)) + geom_boxplot() + xlab('Pickup Hour') +
  ylab('Total Fare Amount ($)') + ggtitle('Total Fare Amount by Hour') + labs(color='Pickup Hour')

# Visualize relationship between trip distance and total fare --> upward linear trend
# We can see from the shading of the dots that there were some low mileage trips that took a
# long time and costed a lot (especially in Manhattan which is understandable)
cab2016 %>%
  ggplot(aes(x=trip_distance, y=total_amount, col=trip_duration)) + geom_point(alpha=0.1) + 
  facet_grid(pickup_borough~.) + ggtitle('Relationship between Fare and Trip Distance') +
  xlab('Trip Distance') + ylab('Total Amount ($)') + labs(color = 'Trip Duration') + 
  scale_color_gradient(low='yellow', high='purple')

# Same plots as above but for total amount by pickup zone - each graph represent a borough
# We are not putting all of them here but there are obvious differences between pickup zones
# so that should be included in the model
# Use zoom function to see plots better
cab2016 %>%
  filter(pickup_borough == "Bronx") %>%
  ggplot(aes(x=pickup_zone, y=total_amount, col=pickup_zone)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab('Pickup Zone') + 
  ylab('Total Amount') + ggtitle('Total Amount by Pickup Zone in the Bronx') + labs(color = 'Pickup Zone')

cab2016 %>%
  filter(pickup_borough == "Brooklyn") %>%
  ggplot(aes(x=pickup_zone, y=total_amount, col=pickup_zone)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab('Pickup Zone') + 
  ylab('Total Amount') + ggtitle('Total Amount by Pickup Zone in Brooklyn') + labs(color = 'Pickup Zone')

# Same plots as above but for total amount by dropoff zone - each graph represent a borough
# We are not putting all of them here but there are obvious differences between dropoff zones
# so that should be included in the model
# Use zoom function to see plots better
cab2016 %>%
  filter(dropoff_borough == "Bronx") %>%
  ggplot(aes(x=dropoff_zone, y=total_amount, col=dropoff_zone)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab('Dropoff Zone') + 
  ylab('Total Amount') + ggtitle('Total Amount by Dropoff Zone in the Bronx') + labs(color = 'Dropoff Zone')

cab2016 %>%
  filter(dropoff_borough == "Brooklyn") %>%
  ggplot(aes(x=dropoff_zone, y=total_amount, col=dropoff_zone)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab('Dropoff Zone') + 
  ylab('Total Amount') + ggtitle('Total Amount by Dropoff Zone in Brooklyn') + labs(color = 'Dropoff Zone')


# Conditions look better than the last one but still not great
total_mod <- lm(total_amount ~ pickup_hour + trip_distance + pickup_zone + dropoff_zone, data=cab2016)
summary(total_mod)
plot(which=1, total_mod)
plot(which=2, total_mod)
plot(which=4, total_mod) # this is very weird, the ones that are really high have perfect predictions

cab2016 <- cab2016 %>%
  mutate(fare_prediction = predict(total_mod, cab2016))

# MODEL BUILDING PART THREE: Trip duration predictions

# Get variable in form that we can work with
cab2016 <- cab2016 %>%
  mutate(trip_duration = as.numeric(trip_duration))

# We see some slight differences here, especially with Queens
cab2016 %>%
  ggplot(aes(x=pickup_borough, y=trip_duration, col=pickup_borough)) + geom_boxplot() + xlab('Pickup Borough') +
  ylab('Trip Duration (mins)') + ggtitle('Trip Duration by Pickup Borough') + labs(color='Pickup Borough') + 
  theme(axis.text.x=element_text(angle=90, hjust=1))

# We see bigger differences here, so we'll definitely use this 
cab2016 %>%
  ggplot(aes(x=dropoff_borough, y=trip_duration, col=dropoff_borough)) + geom_boxplot() + xlab('Dropoff Borough') +
  ylab('Trip Duration (mins)') + ggtitle('Trip Duration by Dropoff Borough') + labs(color='Dropoff Borough') + 
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Trip duration against trip distance colored by the pickup borough
# There is definitely a relationship here but it doesn't seem quite linear
cab2016 %>%
  ggplot(aes(x=trip_distance, y=trip_duration, col=pickup_borough)) + geom_point(alpha=0.4)

# Not showing many plots for pickup zone and dropoff zone here but they look similar to the ones
# in the previous models (one example is below)
cab2016 %>%
  filter(dropoff_borough == "Brooklyn") %>%
  ggplot(aes(x=pickup_zone, y=trip_distance, col=pickup_zone)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab('Pickup Zone') + 
  ylab('Trip Distance (mi)') + ggtitle('Trip Distance by Pickup Zone in Brooklyn') + labs(color = 'Pickup Zone')

# Final model for predicting trip duration (in minutes)
# The conditions still don't look good 
duration_model <- lm(trip_duration ~ pickup_zone + dropoff_zone + pickup_hour + trip_distance, data=cab2016)
summary(duration_model)
plot(which=1, duration_model)
plot(which=2, duration_model)
plot(which=4, duration_model)

# STUFF WE NEED FOR MAPPING
# Prepare shapefile for use
file <- ('our_neighborhoods.geojson')
neighborhood_shape <- readOGR(dsn = file, layer = "our_neighborhoods")
neighborhood_shape_df <- tidy(neighborhood_shape)

# API Key for Google Maps
api_key <- "AIzaSyBMILnxtB-IgmBIjsaxYyZK_Y0LwoOvYIE"

# We need this in order to say the weekday in 2018 for the date that the user chose
nextyear <- data.frame(2018)

# Define UI for application
ui <- shinyUI(fluidPage(theme=shinytheme("slate"), 
  span(titlePanel('CA$H CAB'), style="color:#F9D90A"),
  sidebarLayout(position='right',
    sidebarPanel(
      h4(textOutput('intro')),
      h5(''),
      selectInput('pickup_month', 'Select Month:', choices=as.character(unique(sort(cab2016$pickup_month)))),
      selectInput('pickup_day', 'Select Date:', choices=as.character(unique(sort(cab2016$pickup_day)))),
      selectInput('pickup_hour', 'Select Hour of Pickup:', choices=unique(sort(cab2016$pickup_hour))), 
      uiOutput('pickup_borough'),
      uiOutput('pickup_zone'),
      uiOutput('dropoff_borough'),
      uiOutput('dropoff_zone'),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      span(h3('Predicting NYC Cab Fares and Trip Duration'), style="color:#F9D90A"),
      h4(textOutput("instructions")),
      h3(''),
      leafletOutput("Map1"),
      h4(textOutput("weekday1")),
      h4(textOutput("prediction1")),
      h4(textOutput("prediction2")),
      h5(''),
      h4(textOutput("explanation")),
      h5(''),
      leafletOutput("Map2"),
      h5(''),
      google_mapOutput("myMap")
    )    
  )
)
)

# Define server
server <- function(input, output) {
  output$intro <- renderText("Please fill in the following information to plan a cab ride you wish to
                             take in 2018.")
  output$pickup_borough <- renderUI(selectInput('pickup_borough',
                                          'Select Pickup Borough:', c(unique(sort(cab2016$pickup_borough)))))
  output$pickup_zone <- renderUI(
    if(is.null(input$pickup_borough)) {return()}
    else selectInput('pickup_zone', 'Select Pickup Neighborhood:',
                     c(unique(sort(cab2016$pickup_zone[which(cab2016$pickup_borough == input$pickup_borough)])))))
  
  output$dropoff_borough <- renderUI(selectInput('dropoff_borough',
                                          'Select Dropoff Borough:', c(unique(sort(cab2016$dropoff_borough)))))
  output$dropoff_zone <- renderUI(
    if(is.null(input$dropoff_borough)) {return()}
    else selectInput('dropoff_zone', 'Select Dropoff Neighborhood:',
                     c(unique(sort(cab2016$dropoff_zone[which(cab2016$dropoff_borough == input$dropoff_borough)])))))
  dropdown1 <- reactive(cab2016[which(cab2016$pickup_borough==input$borough),])
  
  dropdown2 <- reactive(dropdown1()[which(dropdown1()$pickup_zone == input$neighborhood),])
  
  dropdown3 <- reactive(cab2016[which(cab2016$dropoff_borough==input$dborough),])
  
  dropdown4 <- reactive(dropdown3()[which(dropdown3()$dropoff_zone == input$dneighborhood),])
  
  output$instructions <- renderText("As you are choosing your pickup and dropoff location, you can refer to the
                                    following map for reference. You can click on a marker and the name of that
                                    neighborhood will appear.")
  output$Map1 <- renderLeaflet(leaflet(data=neighborhood_shape_df) %>%
                                 addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", 
                                          attribution = 'Google') %>%
                                 addMarkers(~coords.x1, ~coords.x2, popup=~name) %>%
                                 setView(-73.98, 40.75, zoom = 12))
  observeEvent(
    input$submit, {
      df.taxi <- data.frame(pickup_borough=input$pickup_borough, pickup_zone=input$pickup_zone, 
                            dropoff_borough=input$dropoff_borough, 
                            dropoff_zone=input$dropoff_zone, pickup_day=input$pickup_day,
                            pickup_hour=as.factor(input$pickup_hour), pickup_month=input$pickup_month)
      df.taxi$weekday <- weekdays(ymd(paste0(nextyear$X2018, "-", as.numeric(input$pickup_month),
                                             "-", as.numeric(input$pickup_day))))
      output$weekday1 <- renderText(paste("The date you chose is a",df.taxi$weekday,"for the year 2018."))
      df.taxi$trip_distance <- predict(distance_mod, df.taxi)
      df.taxi$fare_prediction <- predict(total_mod, df.taxi)
      df.taxi$lower_interval <- predict(total_mod, df.taxi, interval="predict")[2]
      df.taxi$upper_interval <- predict(total_mod, df.taxi, interval="predict")[3]
      output$prediction1 <- renderText(paste("Your trip is expected to cost $",
                                             format(round(df.taxi$fare_prediction, 2), nsmall=2),"but could cost
                                             as much as $",format(round(df.taxi$upper_interval,2), nsmall=2), "."))
      df.taxi$time_prediction <- ceiling(predict(duration_model, df.taxi))
      df.taxi$lower_interval2 <- predict(duration_model, df.taxi, interval="predict")[2]
      df.taxi$upper_interval2 <- predict(duration_model, df.taxi, interval="predict")[3]
      output$prediction2 <- renderText(paste("Your trip is expected to take about", 
                                             format(round(df.taxi$time_prediction, 0), nsmall=0), " minutes but could
                                             last as long as", format(round(df.taxi$upper_interval2,0), nsmall=0), "minutes."))
      mapdata <- cab2016 %>%
        filter(pickup_zone==input$pickup_zone, dropoff_zone==input$dropoff_zone)
      output$explanation <- renderText("The following map is representative of pickups and dropoffs.
                                       The blue dots represent cabs picking up fares in the same neighborhood you
                                       designated for pickup. The red dots represent cabs dropping off in the same 
                                       neighborhood that you designated for drop off. If there are no points, we
                                       have no data in our sample for rides between your specified locations.")
      output$Map2 <- renderLeaflet({
        leaflet(neighborhood_shape) %>%
          addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", 
                   attribution = 'Google') %>%
          addCircleMarkers(~mapdata$pickup_longitude, ~mapdata$pickup_latitude, radius = 1,
                           color = "blue", fillOpacity = 0.1) %>%
          addCircleMarkers(~mapdata$dropoff_longitude, ~mapdata$dropoff_latitude, radius = 1,
                           color = "red", fillOpacity = 0.1) %>%
          addMarkers(~mapdata$n_longitude.x, ~mapdata$n_latitude.x, popup=~mapdata$pickup_zone) %>%
          addMarkers(~mapdata$n_longitude.y, ~mapdata$n_latitude.y, popup=~mapdata$dropoff_zone) %>%
          setView(-73.98, 40.75, zoom = 12)
      })
      # The following google maps output is adapted slightly from
      # https://stackoverflow.com/questions/42026578/drawing-journey-path-using-leaflet-in-r
      
      # Our attempt at restricting the maps to nyc only - we were getting weird routes before this restriction
      df.taxi$region <- paste("new york city", df.taxi$pickup_zone, sep=" ")
      df.taxi$region1 <- paste("new york city", df.taxi$dropoff_zone, sep=" ")
      route <- eventReactive(input$submit,{
        origin <- df.taxi$region
        destination <- df.taxi$region1
        return(data.frame(origin, destination, stringsAsFactors = F))
      })
      output$myMap <- renderGoogle_map({
        selected_route <- route()
        directions <- google_directions(key = api_key,
                                 origin = selected_route$origin,
                                 destination = selected_route$destination)
        df_route <- data.frame(route = directions$routes$overview_polyline$points)
        google_map(key = api_key) %>%
          add_polylines(data = df_route, polyline = "route")
      })
      
    })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)

