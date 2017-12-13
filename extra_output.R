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



output$Map2 <- renderLeaflet({jan2016 %>%
    filter(pickup_hour==input$pickup_hour) %>%
    leaflet(neighborhood_shape1) %>%
    addTiles() %>% 
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(~jan2016$pickup_longitude, ~jan2016$pickup_latitude, radius = 1,
                     color = "blue", fillOpacity = 0.1) %>%
    setView(-73.98, 40.75, zoom = 12)
})



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



output$prediction4 <- renderText(paste("We are 95% confident that your trip will last between", 
                                       format(round(df.taxi$lower_interval2,2), nsmall=2), "and", 
                                       format(round(df.taxi$upper_interval2,2), nsmall=2), "minutes."))


output$prediction2 <- renderText(paste("95% of predicted fares for this time will fall between $", 
                                       format(round(df.taxi$lower_interval,2), nsmall=2), "and $", 
                                       format(round(df.taxi$upper_interval,2), nsmall=2), "."))




#MAPS
map <- leaflet()
map <- addTiles(map)
map <- addMarkers(map, lng=-73.8648, lat=40.8448, popup="Bronx")
map <- addMarkers(map, lng=-73.9442, lat=40.6782, popup="Brooklyn")
map <- addMarkers(map, lng=-73.9712, lat=40.7831, popup="Manhattan")
map <- addMarkers(map, lng=-73.7949, lat=40.7282, popup="Queens")
map


#cab2016$factor_pickup = factor(x = cab2016$pickup_borough,levels = c("Bronx", "Brooklyn", "Manhattan", "Staten Island", "Queens"))
#cab2016$factor_dropoff = factor(x = cab2016$dropoff_borough,levels = c("Bronx", "Brooklyn", "Manhattan", "Staten Island", "Queens"))
