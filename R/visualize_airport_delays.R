#' Visualisation of airport delays 
#'
#' Visualizes the mean delay of flights for different airports by longitude and latitude
#' from the dataset in the package nycflights13.
#'
#' @name visualize_airport_delays
#' @export visualize_airport_delays
#' @return A ggplot visualising the mean delays.
#' 
visualize_airport_delays <- function(){
  
  flights <- nycflights13::flights
  airports <- nycflights13::airports
  
  airport_delays <- flights %>%
    group_by(airport = coalesce(dest, origin)) %>%
    summarise(
      mean_delay = mean(coalesce(arr_delay, dep_delay), na.rm = TRUE)
    ) %>%
    filter(!is.na(mean_delay)) %>%
    bind_rows(
      flights %>%
        group_by(airport = origin) %>%
        summarise(
          mean_delay = mean(dep_delay, na.rm = TRUE)
        )
    ) %>% 
    inner_join(airports, by = c("airport" = "faa")) %>% 
    select(mean_delay, name, lat, lon)
  
  
  ggplot(airport_delays, aes(x = lon, y = lat, color = mean_delay)) +
    geom_point(size = 3) +
    labs(x = "Longitude", y = "Latitude", title = "Mean Delay of Flights by Airport") +
    theme_bw()
}




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


visualize_airport_delays()
