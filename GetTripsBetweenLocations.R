#Function to subset the taxi data such that only locations of interest are considered
#trips: Input data frame
#startpoint: Longitude and Latitude of where the trips of interest should originate
#endpoint: Longitude and Latitude of where the trips should end
#Distance: Permissable distance from the coordinates of interest, default: 50 meters


TripsBetweenPoints<-function(trips,startpoint,endpoint,Distance=50)
{
  
  require(geosphere)
  require(data.table)
  require(dplyr)
    TripsBetweenPoints<- setDT(trips)[ ,StartDistance := distHaversine(matrix(c(StartLon, StartLat), ncol = 2),matrix(c(startpoint), ncol = 2))]
    TripsBetweenPoints<-setDT(trips)[ ,EndDistance := distHaversine(matrix(c(EndLon, EndLat), ncol = 2),matrix(c(endpoint), ncol = 2))]
    TripsBtn<-TripsBetweenPoints%>%
        filter(StartDistance<=Distance,EndDistance<=Distance)
    return(TripsBtn)
}



