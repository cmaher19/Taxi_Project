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


#Load the data
jan2016 <- read.csv('yellowjan2016_sample.csv')

taxi_zone <- read.csv('taxi_zone_lookup.csv')

#DATA WRANGLING

#Convert pickup and dropoff date and time information into date-time objects
jan2016 <- jan2016 %>%
  mutate(tpep_pickup_datetime = as.POSIXct(tpep_pickup_datetime),
         tpep_dropoff_datetime = as.POSIXct(tpep_dropoff_datetime))


#Create a total trip duration variable (in minutes)
jan2016 <- jan2016 %>%
  mutate(trip_duration = difftime(tpep_dropoff_datetime, tpep_pickup_datetime, unit='min'))


#Split date and time pick up and drop off variables into two separate variables each 
#(one for date and one for time)
jan2016 <- jan2016 %>%
  separate(tpep_pickup_datetime, into=c('pickup_date', 'pickup_time'), sep=10) %>%
  separate(tpep_dropoff_datetime, into=c('dropoff_date', 'dropoff_time'), sep=10)


#Remove the extra space in front of time variables
jan2016 <- jan2016 %>%
  mutate(pickup_time = trimws(pickup_time, which='left'),
         dropoff_time = trimws(dropoff_time, which='left'))

#Separate the time variables into hours and minutes and seconds, then factor them
jan2016 <- jan2016 %>%
  separate(pickup_time, into = c('pickup_hour', 'pickup_minute', 'pickup_seconds'), sep=':') %>%
  separate(dropoff_time, into = c('dropoff_hour', 'dropoff_minute', 'dropoff_seconds'), sep=':') %>%
  mutate(pickup_hour = as.factor(pickup_hour), pickup_minute = as.factor(pickup_minute),
         dropoff_hour = as.factor(dropoff_hour), dropoff_minute = as.factor(dropoff_minute)) 


#Convert pickup and dropoff dates to actual date variables
jan2016 <- jan2016 %>%
  mutate(pickup_date = as.Date(pickup_date), dropoff_date = as.Date(dropoff_date))


#Extract weekday from dates so that it can be used in later models/predictions
jan2016 <- jan2016 %>%
  mutate(weekday = weekdays(pickup_date))


# Separate date variables into year, month, and day separately so that they are easier to 
# match with user input
jan2016 <- jan2016 %>%
  separate(pickup_date, into=c('pickup_year', 'pickup_month', 'pickup_day'), sep='-') %>%
  separate(dropoff_date, into=c('dropoff_year', 'dropoff_month', 'dropoff_day'), sep='-')


#Convert month and day to factors for later use in predictive models
jan2016 <- jan2016 %>%
  mutate(pickup_month = as.factor(pickup_month), pickup_day = as.factor(pickup_day),
         dropoff_month = as.factor(dropoff_month), dropoff_day = as.factor(dropoff_day))


#join taxi zone data onto yellow cab data for both pickup and dropoff location
jan2016 <- left_join(jan2016, taxi_zone, by=c('PULocationID'='LocationID')) %>%
  rename(pickup_borough = Borough, pickup_zone=Zone, pickup_servicezone=service_zone)

jan2016 <- left_join(jan2016, taxi_zone, by=c('DOLocationID'='LocationID')) %>%
  rename(dropoff_borough = Borough, dropoff_zone=Zone, dropoff_servicezone=service_zone)


#Filter out observations with no pickup/dropoff location, rides that go to Newark Airport, and rates other 
#then the standard fare rates (there were some shady exchanges in the nonstandard fare options)
jan2016 <- jan2016 %>%
  filter(pickup_borough != 'Unknown') %>%
  filter(dropoff_borough != 'Unknown') %>%
  filter(pickup_borough != 'EWR') %>%
  filter(dropoff_borough != 'EWR') %>%
  filter(pickup_servicezone != 'Airports') %>%
  filter(RatecodeID == 1)

#Change to character because it allows app part to display neighborhood and borough names instead
#of just a number
jan2016 <- jan2016 %>%
  mutate(pickup_borough = as.character(pickup_borough), pickup_zone = as.character(pickup_zone),
         dropoff_borough = as.character(dropoff_borough), dropoff_zone = as.character(dropoff_zone))


#MODEL BUILDING
#Take user input and use it to build predictive models for fare prediction and time duration

#Model to describe total fare amount based on variables that we will have from user input
#Cant put month variable in until we are using full dataset due to small level of factors (i.e. 1)
#First build model to predict trip_distance, then use that prediction in this model

#Visualize relationship between trip distance and total fare --> pretty strong upward linear trend
jan2016 %>%
  ggplot(aes(x=trip_distance, y=total_amount, col=pickup_borough)) + geom_point(alpha=0.2) + 
  facet_grid(pickup_borough~.)

#MODEL: Using trip distance to predict total fare amount, adj R^2 is 0.8954 (yay!)
mod1 <- lm(total_amount ~ trip_distance, data=jan2016)
summary(mod1)

newmod1 <- lm(total_amount ~ pickup_zone + dropoff_zone + pickup_hour
              + weekday + trip_distance, data=jan2016)
summary(newmod1)


#Visualize relationship between trip distance and pickup_zone
#Do that here but also figure out what to do since pickup_zone has hundreds of factors

#Should we filter out the super far trips? May lose some information about some boroughs to far away boroughs
#But it would probably make the distribution a lot more normal
far_trips <- jan2016 %>%
  filter(trip_distance > 18)

#MODEL: Using pickup and dropoff zone to predict trip distance
distance <- lm(trip_distance ~ pickup_zone + dropoff_zone + pickup_hour, data=jan2016)
summary(distance)

jan2016$distance_prediction <- predict(distance, jan2016)
#plot(distance)

#predict on training set
jan2016$total_prediction <- predict(mod1, jan2016)
test_result <- jan2016 %>%
  summarise(MAE = sum(abs(total_prediction - total_amount))/n(),
            MSE = sum((total_prediction - total_amount)/n()),
            SSE = sum((total_prediction - total_amount)^2))
test_result

jan2016$factor_pickup = factor(x = jan2016$pickup_borough,levels = c("Bronx", "Brooklyn", "Manhattan", "Staten Island", "Queens"))
jan2016$factor_dropoff = factor(x = jan2016$dropoff_borough,levels = c("Bronx", "Brooklyn", "Manhattan", "Staten Island", "Queens"))
jan2016$trip_duration = as.numeric(jan2016$trip_duration)

#density of trip_duration
duration_density <- jan2016 %>%
  filter(trip_duration < 60) %>% #made decision to do less than 60 because it skews heavily right with 
  #very low density after that cut off
  ggplot(aes(x=trip_duration)) + geom_density()

jan2016 <- jan2016 %>%
  filter(trip_duration < 60)

#trying to build useful trip duration model
jan2016 %>%
  ggplot(aes(x=pickup_borough, y=trip_duration)) + geom_boxplot()

jan2016 %>%
  ggplot(aes(x=dropoff_borough, y=trip_duration)) + geom_boxplot()

jan2016 %>%
  group_by(dropoff_borough) %>%
  ggplot(aes(x=dropoff_borough, y=trip_duration)) + geom_boxplot()

#final model for predicting trip duration (in minutes)
mod2 <- lm(trip_duration ~ pickup_zone + dropoff_zone + pickup_hour + trip_distance, data=jan2016)
summary(mod2)

# Define UI for application
ui <- shinyUI(fluidPage(
  titlePanel('Ca$h Cab'),
  sidebarLayout(
    sidebarPanel(
      tags$style(".content, .container-fluid {background-color: #8897a6;}"),
      selectInput('pickup_month', 'Select Month', choices=as.character(unique(sort(jan2016$pickup_month)))),
      selectInput('pickup_day', 'Select Date', choices=as.character(unique(sort(jan2016$pickup_day)))),
      selectInput('weekday', 'Select Day of Week', choices=as.character(unique(c('Sunday', 'Monday', 
                                                                                 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')))),
      selectInput('pickup_hour', 'Select Hour of Pickup', 
                  choices=unique(sort(jan2016$pickup_hour))), 
      selectInput('pickup_minute', 'Select Minute of Pickup', 
                  choices=as.character(unique(sort(jan2016$pickup_minute)))),
      uiOutput('PBorough'),
      uiOutput('PNeighborhood'),
      uiOutput('DBorough'),
      uiOutput('DNeighborhood'),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      span(h1('Fare Prediction'), style="color:azure"),
      textOutput("prediction1"),
      textOutput("prediction2"),
      textOutput("prediction3"),
      textOutput("prediction4"),
      h2(''),
      plotOutput("plot2"),
      h3(''),
      plotOutput("plot3"),
      h4(''),
      plotOutput("plot4"),
      h5(''),
      plotOutput("plot5")
    )    
  )
)
)

# Define server
server <- function(input, output) {
  output$PBorough <- renderUI(selectInput('pickup_borough',
                                          'Select Pickup Borough', c(unique(sort(jan2016$pickup_borough))))
  )
  output$PNeighborhood <- renderUI(
    if(is.null(input$pickup_borough)) {return()}
    else selectInput('pickup_zone', 'Select Pickup Neighborhood',
                     c(unique(sort(jan2016$pickup_zone[which(jan2016$pickup_borough == input$pickup_borough)])))
    ))
  
  output$DBorough <- renderUI(selectInput('dropoff_borough',
                                          'Select Dropoff Borough', c(unique(sort(jan2016$dropoff_borough)))
  ))
  output$DNeighborhood <- renderUI(
    if(is.null(input$dropoff_borough)) {return()}
    else selectInput('dropoff_zone', 'Select Dropoff Neighborhood',
                     c(unique(sort(jan2016$dropoff_zone[which(jan2016$dropoff_borough == input$dropoff_borough)])))
    ))
  
  sub1 <- reactive(
    jan2016[which(jan2016$pickup_borough==input$borough),]
  )
  sub2 <- reactive(
    sub1()[which(sub1()$pickup_zone == input$neighborhood),]
  )
  sub3 <- reactive(
    jan2016[which(jan2016$dropoff_borough==input$dborough),]
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
  
  observeEvent(
    input$submit, {
      df.taxi <- data.frame(pickup_borough=input$pickup_borough, pickup_zone=input$pickup_zone, 
                            weekday=input$weekday, dropoff_borough=input$dropoff_borough, 
                            dropoff_zone=input$dropoff_zone, pickup_day=input$pickup_day,
                            pickup_hour=as.factor(input$pickup_hour), 
                            pickup_minute=as.factor(input$pickup_minute))
      df.taxi$trip_distance <- predict(distance, df.taxi)
      df.taxi$fare_prediction <- predict(mod1, df.taxi)
      output$prediction1 <- renderText(paste("The exact fare prediction is $", 
                                             format(round(df.taxi$fare_prediction, 2), nsmall=2), "."))
      df.taxi$lower_interval <- predict(mod1, df.taxi, interval="predict")[2]
      df.taxi$upper_interval <- predict(mod1, df.taxi, interval="predict")[3]
      output$prediction2 <- renderText(paste("95% of predicted fares for this time will fall between $", 
                                             format(round(df.taxi$lower_interval,2), nsmall=2), "and $", 
                                             format(round(df.taxi$upper_interval,2), nsmall=2), "."))
      df.taxi$time_prediction <- ceiling(predict(mod2, df.taxi))
      output$prediction3 <- renderText(paste("The predicted trip duration is", 
                                             format(round(df.taxi$time_prediction, 2), nsmall=2), " minutes."))
      df.taxi$lower_interval2 <- predict(mod2, df.taxi, interval="predict")[2]
      df.taxi$upper_interval2 <- predict(mod2, df.taxi, interval="predict")[3]
      output$prediction4 <- renderText(paste("We are 95% confident that your trip will last between", 
                                             format(round(df.taxi$lower_interval2,2), nsmall=2), "and", 
                                             format(round(df.taxi$upper_interval2,2), nsmall=2), "minutes."))
      
      
      pickup <- levels(df.taxi$pickup_hour)
      output$plot2 <- renderPlot(jan2016 %>%
                                   filter(jan2016$pickup_hour == pickup) %>%
                                   ggplot(aes(x=total_amount)) + geom_density()
                                 + annotate("segment", x=df.taxi$fare_prediction, xend=df.taxi$fare_prediction,
                                            y=0, yend=0.1, color="Purple")
                                 + ggtitle("Distribution of Fares for Chosen Pickup Hour") 
                                 + xlab("Fare Price") + ylab("Density"))
      output$plot3 <- renderPlot(
        jan2016 %>%
          filter(jan2016$pickup_zone == input$pickup_zone) %>%
          ggplot(aes(x=total_amount)) + geom_density() 
        + annotate("segment", x=df.taxi$fare_prediction, xend=df.taxi$fare_prediction,
                   y=0, yend=0.1, color="Purple") 
        + ggtitle("Distribution of Fares for Chosen Pickup Neighborhood") 
        + xlab("Fare Price") + ylab("Density"))
      
      output$plot4 <- renderPlot(jan2016 %>% 
                                   filter(pickup_hour==input$pickup_hour) %>%
                                   ggplot(aes(x=factor_pickup, y=factor_dropoff, fill=total_amount)) + geom_tile() + 
                                   labs(x = 'Pickup Borough', y = 'Dropoff Borough',
                                        title='Predicted Fare for Trips in the Designated Pickup Hour') + 
                                   scale_fill_gradient(low = 'white', high = 'darkblue') + 
                                   theme(plot.title = (element_text(size=20, hjust = 0))) +
                                   theme(plot.subtitle = (element_text(size=20, hjust = 0.5))) +
                                   theme(axis.text.x = element_text(size=10)) +
                                   theme(axis.text.y = element_text(size=10)) +
                                   theme(axis.title.x = element_text(size=26)) +
                                   theme(axis.title.y = element_text(size=26)) +
                                   theme(panel.background = element_rect(fill = 'white'),
                                         panel.border = element_rect(colour = 'black', fill=NA, size=1),
                                         legend.background = element_rect(fill='gray90', size=1))
      )
      
      output$plot5 <- renderPlot(jan2016 %>%
                                   filter(pickup_hour==input$pickup_hour) %>%
                                   ggplot(aes(x=factor_pickup, y=factor_dropoff, fill=trip_duration)) + geom_tile() + 
                                   labs(x = 'Pickup Borough', y = 'Dropoff Borough', 
                                        title='Trip Duration for the Designated Pickup Hour') + 
                                   scale_fill_gradient(low = 'white', high = 'darkblue') + 
                                   theme(plot.title = (element_text(size=20, hjust = 0))) +
                                   theme(plot.subtitle = (element_text(size=20, hjust = 0.5))) +
                                   theme(axis.text.x = element_text(size=10)) +
                                   theme(axis.text.y = element_text(size=10)) +
                                   theme(axis.title.x = element_text(size=26)) +
                                   theme(axis.title.y = element_text(size=26)) +
                                   theme(panel.background = element_rect(fill = 'white'),
                                         panel.border = element_rect(colour = 'black', fill=NA, size=1),
                                         legend.background = element_rect(fill='gray90', size=1))
                                 
      )
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



