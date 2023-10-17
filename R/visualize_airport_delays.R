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



