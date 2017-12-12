#
# This is a Shiny web application. You can run the application by clicking
# the ‘Run App’ button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Load necessary packages
library(mdsr)
library(shiny)
library(tidyverse)
library(leaflet)
library(tigris)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)


#Load the data
jan2016 <- read.csv('jan1.csv')
feb2016 <- read.csv('feb1.csv')
mar2016 <- read.csv('mar1.csv')
apr2016 <- read.csv('apr1.csv')
may2016 <- read.csv('may1.csv')
jun2016 <- read.csv('jun1.csv')
cab2016 <- rbind(jan2016, feb2016, mar2016, apr2016, may2016, jun2016)
centroids <- read.csv('NHoodNameCentroids.csv')

#clean centroid data
centroids$longitude.latitude.section.borough <- as.character(centroids$longitude.latitude.section.borough)

centroids <- centroids %>%
  separate(longitude.latitude.section.borough, into=c('n_longitude', 'n_latitude', 'neighborhood', 'borough'), sep=';') %>%
  mutate(n_longitude = as.numeric(n_longitude), n_latitude = as.numeric(n_latitude))

#create a file with the taxi pickup and dropoff locations
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

distance <- lapply(m,n, FUN=pickup_distance)
distance <- as.data.frame(distance)
colnames(distance) = centroids[,3]
pickup_nhood <- colnames(distance)[apply(distance, 1, which.min)]
pickup_nhood <- as.data.frame(pickup_nhood)

# Find nearest neighborhood for dropoff based on coordinates
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
cab2016 <- cbind(cab2016, neighborhoods)

#join neighborhood centroid data onto yellow cab data for both pickup and dropoff location
cab2016 <- left_join(cab2016, centroids, by=c('pickup_nhood'='neighborhood')) %>%
  rename(pickup_borough = borough, pickup_zone=pickup_nhood) %>%
  select(-n_longitude, -n_latitude)


cab2016 <- left_join(cab2016, centroids, by=c('dropoff_nhood'='neighborhood')) %>%
  rename(dropoff_borough = borough, dropoff_zone=dropoff_nhood) %>%
  select(-n_longitude, -n_latitude)


#DATA WRANGLING

#Convert pickup and dropoff date and time information into date-time objects
cab2016 <- cab2016 %>%
  mutate(tpep_pickup_datetime = as.POSIXct(tpep_pickup_datetime),
         tpep_dropoff_datetime = as.POSIXct(tpep_dropoff_datetime))


#Create a total trip duration variable (in minutes)
cab2016 <- cab2016 %>%
  mutate(trip_duration = difftime(tpep_dropoff_datetime, tpep_pickup_datetime, unit='min'))


#Split date and time pick up and drop off variables into two separate variables each 
#(one for date and one for time)
cab2016 <- cab2016 %>%
  separate(tpep_pickup_datetime, into=c('pickup_date', 'pickup_time'), sep=10) %>%
  separate(tpep_dropoff_datetime, into=c('dropoff_date', 'dropoff_time'), sep=10)


#Remove the extra space in front of time variables
cab2016 <- cab2016 %>%
  mutate(pickup_time = trimws(pickup_time, which='left'),
         dropoff_time = trimws(dropoff_time, which='left'))

#Separate the time variables into hours and minutes and seconds, then factor them
cab2016 <- cab2016 %>%
  separate(pickup_time, into = c('pickup_hour', 'pickup_minute', 'pickup_seconds'), sep=':') %>%
  separate(dropoff_time, into = c('dropoff_hour', 'dropoff_minute', 'dropoff_seconds'), sep=':') %>%
  mutate(pickup_hour = as.factor(pickup_hour), pickup_minute = as.factor(pickup_minute),
         dropoff_hour = as.factor(dropoff_hour), dropoff_minute = as.factor(dropoff_minute)) 


#Convert pickup and dropoff dates to actual date variables
cab2016 <- cab2016 %>%
  mutate(pickup_date = as.Date(pickup_date), dropoff_date = as.Date(dropoff_date))


#Extract weekday from dates so that it can be used in later models/predictions
cab2016 <- cab2016 %>%
  mutate(weekday = weekdays(pickup_date))


# Separate date variables into year, month, and day separately so that they are easier to 
# match with user input
cab2016 <- cab2016 %>%
  separate(pickup_date, into=c('pickup_year', 'pickup_month', 'pickup_day'), sep='-') %>%
  separate(dropoff_date, into=c('dropoff_year', 'dropoff_month', 'dropoff_day'), sep='-')


#Convert month and day to factors for later use in predictive models
cab2016 <- cab2016 %>%
  mutate(pickup_month = as.factor(pickup_month), pickup_day = as.factor(pickup_day),
         dropoff_month = as.factor(dropoff_month), dropoff_day = as.factor(dropoff_day))


# Filter out observations with pickup/dropoff coordinates with zeroes and rates other 
# than the standard fare rates (there were some shady exchanges in the nonstandard fare options)
cab2016 <- cab2016 %>%
  filter(RatecodeID == 1, pickup_longitude != 0, dropoff_longitude != 0, total_amount < 2000, total_amount > 0)


#MODEL BUILDING
#Take user input and use it to build predictive models for fare prediction and time duration

#Model to describe total fare amount based on variables that we will have from user input
#Cant put month variable in until we are using full dataset due to small level of factors (i.e. 1)
#First build model to predict trip_distance, then use that prediction in this model

#Visualize relationship between trip distance and total fare --> pretty strong upward linear trend
cab2016 %>%
  ggplot(aes(x=trip_distance, y=total_amount, col=pickup_borough)) + geom_point(alpha=0.2) + 
  facet_grid(pickup_borough~.)

#Should we filter out the super far trips? May lose some information about some boroughs to far away boroughs
#But it would probably make the distribution a lot more normal
ggplot(cab2016, aes(x=trip_distance)) + geom_density()

cab2016 <- cab2016 %>%
  filter(trip_distance < 20)

#MODEL: Using trip distance to predict total fare amount, adj R^2 is 0.8954 (yay!)
#mod1 <- lm(total_amount ~ trip_distance, data=cab2016)
#summary(mod1)

newmod1 <- lm(total_amount ~ pickup_zone + dropoff_zone + pickup_hour
              + weekday + trip_distance, data=cab2016)
summary(newmod1)

#Visualize relationship between trip distance and pickup_zone
#Do that here but also figure out what to do since pickup_zone has hundreds of factors


#MODEL: Using pickup and dropoff zone to predict trip distance
distance_mod <- lm(trip_distance ~ pickup_zone + dropoff_zone + pickup_hour, data=cab2016)
summary(distance_mod)

cab2016$distance_prediction <- predict(distance_mod, cab2016)
#plot(distance)



#cab2016$factor_pickup = factor(x = cab2016$pickup_borough,levels = c("Bronx", "Brooklyn", "Manhattan", "Staten Island", "Queens"))
#cab2016$factor_dropoff = factor(x = cab2016$dropoff_borough,levels = c("Bronx", "Brooklyn", "Manhattan", "Staten Island", "Queens"))
cab2016$trip_duration = as.numeric(cab2016$trip_duration)

#density of trip_duration
cab2016 %>%
  filter(trip_duration < 60) %>% #made decision to do less than 60 because it skews heavily right with 
  #very low density after that cut off
  ggplot(aes(x=trip_duration)) + geom_density()

cab2016 <- cab2016 %>%
  filter(trip_duration < 60)

#trying to build useful trip duration model
cab2016 %>%
  ggplot(aes(x=pickup_borough, y=trip_duration)) + geom_boxplot()

cab2016 %>%
  ggplot(aes(x=dropoff_borough, y=trip_duration)) + geom_boxplot()

cab2016 %>%
  group_by(dropoff_borough) %>%
  ggplot(aes(x=dropoff_borough, y=trip_duration)) + geom_boxplot()

#final model for predicting trip duration (in minutes)
mod2 <- lm(trip_duration ~ pickup_zone + dropoff_zone + pickup_hour + trip_distance + pickup_month, data=cab2016)
summary(mod2)

# Prepare shapefile for use
file <- ('our_neighborhoods.geojson')
neighborhood_shape <- readOGR(dsn = file, layer = "our_neighborhoods")
neighborhood_shape_df <- tidy(neighborhood_shape)


# Define UI for application
ui <- shinyUI(fluidPage(
  span(titlePanel('Ca$h Cab'), style="color:#F9D90A"),
  sidebarLayout(
    sidebarPanel(
      tags$style(".content, .container-fluid {background-color: #8897a6;}"),
      selectInput('pickup_month', 'Select Month', choices=as.character(unique(sort(cab2016$pickup_month)))),
      selectInput('pickup_day', 'Select Date', choices=as.character(unique(sort(cab2016$pickup_day)))),
      selectInput('weekday', 'Select Day of Week', choices=as.character(unique(c('Sunday', 'Monday', 
                                                                                 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')))),
      selectInput('pickup_hour', 'Select Hour of Pickup', 
                  choices=unique(sort(cab2016$pickup_hour))), 
      selectInput('pickup_minute', 'Select Minute of Pickup', 
                  choices=as.character(unique(sort(cab2016$pickup_minute)))),
      uiOutput('PBorough'),
      uiOutput('PNeighborhood'),
      uiOutput('DBorough'),
      uiOutput('DNeighborhood'),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      span(h2('NYC Cab Fares and Trip Duration'), style="color:#F9D90A"),
      span(h3('A Predictive Model'), style="color:#F9D90A"),
      h5(textOutput("instructions")),
      h3(''),
      leafletOutput("Map1"),
      h5(textOutput("prediction1")),
      h5(textOutput("prediction3")),
      h5(textOutput("prediction4")),
      h5(''),
      h5(textOutput("explanation")),
      h6(''),
      leafletOutput("Map2")
    )    
  )
)
)

# Define server
server <- function(input, output) {
  output$PBorough <- renderUI(selectInput('pickup_borough',
                                          'Select Pickup Borough', c(unique(sort(cab2016$pickup_borough))))
  )
  output$PNeighborhood <- renderUI(
    if(is.null(input$pickup_borough)) {return()}
    else selectInput('pickup_zone', 'Select Pickup Neighborhood',
                     c(unique(sort(cab2016$pickup_zone[which(cab2016$pickup_borough == input$pickup_borough)])))
    ))
  
  output$DBorough <- renderUI(selectInput('dropoff_borough',
                                          'Select Dropoff Borough', c(unique(sort(cab2016$dropoff_borough)))
  ))
  output$DNeighborhood <- renderUI(
    if(is.null(input$dropoff_borough)) {return()}
    else selectInput('dropoff_zone', 'Select Dropoff Neighborhood',
                     c(unique(sort(cab2016$dropoff_zone[which(cab2016$dropoff_borough == input$dropoff_borough)])))
    ))
  
  sub1 <- reactive(
    cab2016[which(cab2016$pickup_borough==input$borough),]
  )
  sub2 <- reactive(
    sub1()[which(sub1()$pickup_zone == input$neighborhood),]
  )
  sub3 <- reactive(
    cab2016[which(cab2016$dropoff_borough==input$dborough),]
  )
  sub4 <- reactive(
    sub3()[which(sub3()$dropoff_zone == input$dneighborhood),]
  )
  
  
  renderTable({
    if(is.null(input$borough) || is.null(input$neighborhood)) {return()}
    else return (sub2())
  })
  
  renderTable({
    if(is.null(input$dborough) || is.null(input$dneighborhood)) {return()}
    else return (sub4())
  })
  output$instructions <- renderText("As you are choosing your pickup and dropoff location, you can refer to the
                                    following map for reference. You can click on a marker and the name of that
                                    neighborhood will appear.")
  output$Map1 <- renderLeaflet({
    leaflet(neighborhood_shape) %>%
      addTiles() %>% 
      addMarkers(~coords.x1, ~coords.x2, popup = ~name, data=neighborhood_shape_df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-73.98, 40.75, zoom = 12)
  })
  
  observeEvent(
    input$submit, {
      df.taxi <- data.frame(pickup_borough=input$pickup_borough, pickup_zone=input$pickup_zone, 
                            weekday=input$weekday, dropoff_borough=input$dropoff_borough, 
                            dropoff_zone=input$dropoff_zone, pickup_day=input$pickup_day,
                            pickup_hour=as.factor(input$pickup_hour), 
                            pickup_minute=as.factor(input$pickup_minute), pickup_month=input$pickup_month)
      df.taxi$trip_distance <- predict(distance_mod, df.taxi)
      df.taxi$fare_prediction <- predict(newmod1, df.taxi)
      df.taxi$lower_interval <- predict(newmod1, df.taxi, interval="predict")[2]
      df.taxi$upper_interval <- predict(newmod1, df.taxi, interval="predict")[3]
      output$prediction1 <- renderText(paste("Your trip is expected to cost $", 
                                             format(round(df.taxi$fare_prediction, 2), nsmall=2), ", but could cost
                                             as much as $",format(round(df.taxi$upper_interval,2), nsmall=2), "."))
      df.taxi$time_prediction <- ceiling(predict(mod2, df.taxi))
      df.taxi$lower_interval2 <- predict(mod2, df.taxi, interval="predict")[2]
      df.taxi$upper_interval2 <- predict(mod2, df.taxi, interval="predict")[3]
      output$prediction3 <- renderText(paste("Your trip is expected to take about", 
                                             format(round(df.taxi$time_prediction, 0), nsmall=0), " minutes but could
                                             last as long as", format(round(df.taxi$upper_interval2,0), nsmall=0), "minutes."))
      mapdata <- cab2016 %>%
        filter(pickup_zone==input$pickup_zone, dropoff_zone==input$dropoff_zone, pickup_hour == input$pickup_hour)
      
      output$explanation <- renderText("The following map is representative of the hour your chose for pickup.
                                       The blue dots represent cabs picking up fares in the same neighborhood you
                                       designated for pickup. The red dots represent cabs dropping off in the same 
                                       neighborhood that you designated for drop off.")
      output$Map2 <- renderLeaflet({
        leaflet(neighborhood_shape) %>%
          addTiles() %>% 
          addProviderTiles("CartoDB.Positron") %>%
          addCircleMarkers(~mapdata$pickup_longitude, ~mapdata$pickup_latitude, radius = 1,
                           color = "blue", fillOpacity = 0.1) %>%
          addCircleMarkers(~mapdata$dropoff_longitude, ~mapdata$dropoff_latitude, radius = 1,
                           color = "red", fillOpacity = 0.1) %>%
          setView(-73.98, 40.75, zoom = 12)
      })
      
    })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)



