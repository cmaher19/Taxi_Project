# If you just click the Run App button, this should work.

# TECHNICAL REPORT ABSTRACT: Through our examination of NYC taxi data, we sought to accurately predict a fare amount and a 
# trip duration for a user based on the given information of pickup location, drop-off location, date, and time of day. 
# We were interested in modernizing the cab service to make it comparable to ridesharing companies like Uber and Lyft. 
# To achieve this, we used a Shiny Application that takes in several parameters from a user, inputs them into a model, 
# and outputs a predicted value for cost and duration. In addition to a singular predicted value and a prediction interval, 
# the Shiny output includes dynamic mapping features displaying the locations of neighborhoods, the starting and ending 
# points in selected neighborhoods, and the trip route with an interactive street view. Our motivations aligned more with 
# the wrangling of the data and the final deliverable than the accuracy of the predictions. Overall, we successfully created 
# an interactive Shiny Application that included several dynamic features. 

# TECHNICAL REPORT INTRODUCTION: As aspiring young professionals, many of us may find ourselves living in New York City 
# during the summer for internships and other career-driven endeavors. NYC is one of the most expensive cities to live in, 
# and residents must consider several expenses such as food, rent, and transportation. In recent years, ridesharing 
# companies like Lyft and Uber have become very popular, competing with the long-standing and traditional cab service. 
# The modern characteristics of these up-and-coming transportation services give them an edge; mobile applications are 
# available that allow users to “order” a ride, and they are given their estimated trip duration (through display of ETA) 
# and the cost of their trip. Cabs are not as technologically-driven, and this motivated the ideas behind our project. 

# Using NYC-based data, our ultimate goal was to determine whether we could accurately predict trip duration and total 
# fare based on user input, and whether we could provide a mapping feature for the user. To tackle the predictions, we 
# first explored relationships between variables then proceeded to create linear models. In addition, we created confidence 
# intervals for these predictions to include reasonable uncertainty. For the maps, we used a Google Maps API to make our 
# app more advanced and interesting. With these methods, we were able to create three maps. The first uses markers that 
# represent all of the neighborhoods in the boroughs that the user can choose for their origin and/or destination. The 
# second map depends on the user’s input, displaying specific pickup and drop-off locations within the chosen neighborhoods. The final map also relies on the user’s input, displaying the trip route from the origin to the destination. This map also has an additional interactive component, allowing the user to use the popular street-view feature of Google Maps on and around their selected route.


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

# TECHNICAL REPORT DATA: We used a dataset on yellow cabs in New York City that was collected and provided to the NYC 
# Taxi and Limousine Commission (TLC) by technology providers authorized under the Taxicab and Livery Passenger Enhancement 
# Programs (TPEP/LPEP). This particular dataset included 19 variables, however for our purposes we were mostly interested 
# in eight of them: tpep_pickup_datetime (date and time when meter was engaged), tpep_dropoff_datetime (date and time when 
# meter was disengaged), trip_distance (in miles), pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude, 
# and total_amount (total amount in dollars charged to passengers– considers total fare, miscellaneous extras and surcharges, 
# tax, tolls, and credit card tips). Throughout our process of wrangling the data, we also created several new variables to 
# aid our analysis: trip_duration (using tpep_pickup_datetime and tpep_dropoff_datetime), weekday (using weekdays() function), 
# and “subsetted” variables like pickup_month, pickup_day, and pickup_hour, which we created by separating and modifying 
# existing variables. We also changed the structure of several variables because they were not initially compatible with 
# our model building and Shiny App (reactive drop-down menus required variables of specific structures in order to output 
# desired names). 

# Since the data came in a seaparte file for each month, and each file was roughly 1.5 GB, we had to take a random sample
# of each file in order to read them into R and work with them. We did this by downloading each file onto a school computer
# and randomly sampling using Perl through the Mac's terminal. After some trial and error, we got about 8,000 observations
# (roughly 0.7% of each file) per month, which seemed to be the largest we could get while not crashing R everytime 
# we ran a model. 

# The initial sample data we were using (January 2017 data) had information about pickup and drop-off neighborhoods, 
# so we built the app with this information in mind. When we accessed and began using the data from 2016 (January–June), 
# we realized that neighborhood information was omitted and we only had latitude and longitude. In order to use all of the 
# work we had already completed, we had to find a way to translate these geographical coordinates into neighborhoods. 
# We found a dataset that had latitude and longitude for the center of each neighborhood in NYC, so we decided to use the 
# distance formula to calculate the distance (in miles) between the ride pickup and each neighborhood centroid, took the 
# minimum, and assigned that value as the pickup neighborhood; the same protocol followed for dropoffs. We were then able 
# to use our app in our original designed layout. 

######### TECHNICAL REPORT: READING IN DATA, WRANGLING, UNIVARIATE ANALYSIS, MODEL BUILDING #################

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

# There are four neighborhoods names that exist in two places; This is causing double counting issues
repeat_neighborhoods <- centroids %>%
  group_by(neighborhood) %>%
  summarise(n=n()) %>%
  filter(n>1)

# For Chelsea and Murray Hill, keep Manhattan location; For Sunnyside and Bay Terrace, keep Queens location
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

#Join cab data onto centroid data; Warning message produced here is fine and actually does some of our work for us
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

# Split the data and time aspects into separate variables; Trim extra space from the front of the time variable; 
# Convert pickup date into actual date variable
cab2016 <- cab2016 %>%
  separate(tpep_pickup_datetime, into=c('pickup_date', 'pickup_time'), sep=10) %>%
  separate(tpep_dropoff_datetime, into=c('dropoff_date', 'dropoff_time'), sep=10) %>%
  mutate(pickup_time = trimws(pickup_time, which='left'),
         dropoff_time = trimws(dropoff_time, which='left'))

# Separate the time variables into hours and minutes and seconds, then factor them; 
# Never used seconds or minutes so remove
cab2016 <- cab2016 %>%
  separate(pickup_time, into = c('pickup_hour', 'pickup_minute', 'pickup_seconds'), sep=':') %>%
  separate(dropoff_time, into = c('dropoff_hour', 'dropoff_minute', 'dropoff_seconds'), sep=':') %>%
  mutate(pickup_hour = as.factor(pickup_hour), dropoff_hour = as.factor(dropoff_hour)) %>%
  select(-pickup_minute, -pickup_seconds, -dropoff_minute, -dropoff_seconds)

# Convert pickup and dropoff dates to actual date variables so that weekday can be extracted
cab2016 <- cab2016 %>%
  mutate(pickup_date = as.Date(pickup_date), dropoff_date = as.Date(dropoff_date)) %>%
  mutate(weekday = weekdays(pickup_date))

# Separate date variables into year, month, and day separately to match with user input;
# Convert to factors for model building
cab2016 <- cab2016 %>%
  separate(pickup_date, into=c('pickup_year', 'pickup_month', 'pickup_day'), sep='-') %>%
  separate(dropoff_date, into=c('dropoff_year', 'dropoff_month', 'dropoff_day'), sep='-') %>%
  mutate(pickup_month = as.factor(pickup_month), pickup_day = as.factor(pickup_day),
         dropoff_month = as.factor(dropoff_month), dropoff_day = as.factor(dropoff_day))

#Filter out observations with pickup/dropoff coordinates with zeroes and rates other than the standard fare rates 
# (there were some shady exchanges in the nonstandard fare options); Remove variables we didn't use (the payment 
# variables were summed in the total amount, so we removed the individual charges)
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
# Some of these are literally not possible (the distance between Midtown and Chelsea could not possibly be zero)
cab2016 %>%
  filter(trip_distance==0 & pickup_zone != dropoff_zone)

cab2016 %>%
  filter(trip_distance > 20) 

cab2016 <- cab2016 %>%
  filter(trip_distance != 0 & trip_distance < 20)


# MODEL BUILDING 
# End goal: Take user input and use it to build predictive models for fare prediction and time duration

# MODEL BUILDING PART ONE: trip distance (this wasn't one of the two model we said we'd build but it is useful to 
# use in conjunction with the other models)

# Graphs broken down by borough to make it more readable; There are clear differences between zones 
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

# Same graphs but for dropoff zone instead of pickup zone; You have to zoom these graphs to see them in full
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

# Visualize relationship between trip distance and total fare; Shading of the dots shows that there 
# were some low mileage trips that took a long time and costed a lot (especially in Manhattan)
cab2016 <- cab2016 %>%
  mutate(trip_duration = as.numeric(trip_duration))

cab2016 %>%
  ggplot(aes(x=trip_distance, y=total_amount, col=trip_duration)) + geom_point(alpha=0.1) + 
  facet_grid(pickup_borough~.) + ggtitle('Relationship between Fare and Trip Distance') +
  xlab('Trip Distance') + ylab('Total Amount ($)') + labs(color = 'Trip Duration') + 
  scale_color_gradient(low='yellow', high='purple')

# Same plots as above but for total amount by pickup zone - each graph represent a borough; 
# Not graphing all of them but there are obvious differences between pickup zones (use zoom 
# function to see plots better)
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

# Same as above but for total amount by dropoff zone; Again, differences exist between dropoff zones
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
plot(which=4, total_mod)

cab2016 <- cab2016 %>%
  mutate(fare_prediction = predict(total_mod, cab2016))

# MODEL BUILDING PART THREE: Trip duration predictions

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

# Trip duration against trip distance colored by the pickup borough; Definitely a relationship but it doesn't 
# seem quite linear
cab2016 %>%
  ggplot(aes(x=trip_distance, y=trip_duration, col=pickup_borough)) + geom_point(alpha=0.4)

# Not showing all plots for pickup and dropoff zone but they look similar to those from previous models
cab2016 %>%
  filter(pickup_borough == "Brooklyn") %>%
  ggplot(aes(x=pickup_zone, y=trip_distance, col=pickup_zone)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab('Pickup Zone') + 
  ylab('Trip Distance (mi)') + ggtitle('Trip Distance by Pickup Zone in Brooklyn') + labs(color = 'Pickup Zone')

# Final model for predicting trip duration (in minutes); Conditions look awful 
duration_model <- lm(trip_duration ~ pickup_zone + dropoff_zone + pickup_hour + trip_distance, data=cab2016)
summary(duration_model)
plot(which=1, duration_model)
plot(which=2, duration_model)
plot(which=4, duration_model)

cab2016 <- cab2016 %>%
  mutate(duration_prediction = predict(duration_model, cab2016))

# TECHNICAL REPORT RESULTS SECTION: Since all of our models include pickup and dropoff zone, they are very complex.
# Therefore, it is hard to interpret individual coefficients. For each model we built, the following interpretation holds.
# As an example, I will describe the total amount model. If a pickup zone has a positive coefficient, that means that it is
# expected to cost more when picked up in that zone than whatever the reference zone integrated into the intercept would
# cost. This is assuming that all other variables are held constant. If a pickup zone has a negative coefficient, it is 
# expected to cost less for a trip in comparison to the reference pickup zone. The same interpretation holds for dropoff zones.
# Additonally, the interpretations for the trip duration and trip distance models would be very similar. These models were
# not built to be interpreted coefficient by coefficient. They were built for predictive purposes rather than explanatory
# purposes. To our surprise, adding which day of the week it was didn't seem to add anything to the model. We thought that
# weekdays versus weekends may have had an effect of total cost and trip duration but we didn't see any such effects in 
# the model. Though we've explained a bit about our model building efforts here, I think that the main "results" of our
# project were involved in successfully creating the Shiny app and learning new coding skills through the process.
# Therefore, we will touch on that in the conclusion, which can act as part of this results section as well.



# TECHNICAL REPORT DIAGNOSTICS SECTION: If you go back and run the diagnostic plots for each of the three models we built,
# they are all quite awful. The conditions for linear models certainly are not met. Our residuals plots don't seem to be
# normally distributed, our QQ plots have major normality issues in the tails, and we had points for each model that had 
# concerning Cook's distance values (we measure 'concerning' as greater than 0.05). Going back to the visualizations before 
# we built the models, you can see that there are many high outliers in the data. We did not want to remove these, because 
# those are often from outer boroughs and we already had limited information on those boroughs. Additionally, these models 
# were very far off for permutations of neighborhoods that we did not have in our data. Had we been able to access all of 
# the data available, this problem probably would not have occurred to the same extent. After running tests using our Shiny 
# app, we found that predictions were pretty accurate for neighborhoods in Manhattan that are had a lot of data. Essentially, 
# the more data we had, the better the prediction was. Our models did an awful job of predicting for neighborhoods that had 
# limited data associated with them. Also, we have to be cautious because we are using the prediction from the distance model
# as a variable in our other models, so if our distance model is inaccurate, then the other predictions are affected as well.
# Our models are not good with this amount of data and we should be careful to read too far into measures such as adjusted
# R^2 when looking at our models.

# MAPPING
# Prepare shapefile for use
file <- ('our_neighborhoods.geojson')
neighborhood_shape <- readOGR(dsn = file, layer = "our_neighborhoods")
neighborhood_shape_df <- tidy(neighborhood_shape)

# API Key for Google Maps
api_key <- "AIzaSyBMILnxtB-IgmBIjsaxYyZK_Y0LwoOvYIE"

# We need this in order to say the weekday in 2018 for the date that the user chose
nextyear <- data.frame(2018)

###################################### TECHNICAL REPORT END DATA SECTION ###############################################


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


# TECHNICAL REPORT CONCLUSION SECTION: Ultimately, we were able to create a usable end product in the 
# form of a Shiny Application. The user is able to input the time, date, and place where they would like
# to be picked up as well as their destination. The app returns a predicted fare for their trip as well
# as an estimated trip duration. We also included a form of uncertainty (“your trip could last as long 
# as …” or “your trip could cost as much as …”) so the user is not surprised if their actual fare or trip
# duration are more than the singular predicted values. Additionally, we were able to successfully
# visualize the distribution of neighborhoods with a map, and we were also able to return visualizations
# of the selected trip route for the user.  
#
# We experienced several limitations throughout the course of this project, mostly stemming from the size
# and depth of our dataset. The biggest restriction that limited the scope of our findings was the amount
# of data we were actually able to use in R. As mentioned before, we were only able to use roughly 0.7% of
# the total data, due to the sheer size of the files and capabilities of R. This relatively small amount
# of data affected the accuracy of our predictions; the more combinations of pickup and dropoff 
# neighborhoods we were able to use, the more accurate our predictions would become. Since we were not
# able to use a lot of data, we would expect that using more and ideally all of the data would result in
# much more accurate predictions. If we were able to use all of the data, we would expect our models to
# no longer have rank deficit fits. 
#
# We also had to make some subjective decisions when joining our original dataset with the centroid data
# for the neighborhood locations. There were four instances where two boroughs had neighborhoods with the
# same name which was problematic when joining the datasets because excessive rows and repeated information
# were created for these specific neighborhoods. We ultimately chose to keep the borough locations for the
# neighborhood with more observations, which meant keeping the Manhattan locations for Chelsea and Murray
# Hill, and the Queens locations for Sunnyside and Bay Terrace. These decisions were necessary, but limited
# the scope of our project because we ultimately had to remove four neighborhoods from the dataset.
#
# Additionally, our method to translate latitude and longitude into neighborhoods was our best option,
# but it was not the most effective or accurate because the highest density of pickups in a neighborhood
# is not necessarily at the designated center. As a result, the distance prediction was off, which in turn
# affected the predictions for trip duration and total fare because distance was included in both models.
# This discrepancy can be visualized by running the app and setting the pickup location to Washington 
# Heights in Manhattan and the dropoff location to Morningside Heights in Manhattan. You can see that the
# placement of the markers for these two neighborhoods are not close to the concentration of the most
# frequent pickup and dropoff locations within those areas. 
#
# If we had more time, we could have utilized the Google Maps API further to make our predictions more
# accurate. The Google Maps API has interactive features that allows users to enter specific addresses,
# which we realized very late in the project. If we had known this earlier, the user could have input
# addresses for pickup and dropoff, and our predicted value of trip distance would have been nearly 
# perfect. Since trip distance was an important variable for the other predictions, this would have
# improved the other models and made our predictions much more accurate, and more resemblant of the 
# applications for Lyft and Uber. All of this being said, we are happy with all that we were able 
# do in terms of data wrangling and learning Shiny throughout the course of this project. We are
# happy with the models we built for when they were predicting popular trips (such as Midtown
# to Chelsea in Manhattan) and extremely cautious about taking the predictions with more than a grain of 
# salt for neighborhoods with limited data.


