#' Visualisation of airport delays 
#'
#' Visualizes the mean delay of flights for different airports by longitude and latitude
#' from the dataset in the package nycflights13.
#'
#' @name visualize_airport_delays
#' @import dplyr
#' @import ggplot2
#' @export visualize_airport_delays
#' @return A ggplot visualising the mean delays.
#' 
visualize_airport_delays <- function(){
  flights <- nycflights13::flights
  airports <- nycflights13::airports
  
  # Divide flights in two different data sets, arrive and depart.
  flights_arr <- 
    flights %>% 
    select(c(dep_delay, origin)) %>% 
    rename("delay" = "dep_delay", "airport" = "origin")
  
  flights_dep <- 
    flights %>% 
    select(c(arr_delay, dest)) %>%
    rename("delay" = "arr_delay", "airport" = "dest")
  
  # Combines the delay to one data frame
  delays <- bind_rows(flights_arr, flights_dep)
  
  # Calculates mean delay per airport
  airport_delay <-
    delays %>% 
    group_by(airport) %>% 
    summarise(mean_delay = mean(delay, na.rm=TRUE))
  
  # Add latitude and longitude for the airports and remove airports with NA. 
  airport_delay <- 
    left_join(airport_delay, 
              unique(airports[, c("faa", "lat", "lon")]),
              by =c("airport" = "faa")) %>%
    na.omit()
  
  # Visualise the mean delay
  ggplot(airport_delay, aes(x = lon, y = lat, color = mean_delay)) +
    geom_point(size = 3) +
    labs(x = "Longitude", y = "Latitude", title = "Mean Delay of Flights by Airport") +
    theme_bw()
}

