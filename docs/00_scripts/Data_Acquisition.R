################################ Business Data Science Basics ################################
# Chapter 3 Data Acquisition: Challenge.

library(RSQLite)
library(dplyr)
library(httr)
library(glue)
library(jsonlite)
library(keyring)
library(rvest)
library(stringr)
library(purrr)
library(xopen)
library(stringi)
library(tibble)
library(owmr)

################################################## Task_1 ##################################################

Sys.setenv(OWM_API_KEY = "c457774b4dddc07ff65800bdbf885f5f")
# get current weather data by city name
(res <- get_current("London", units = "metric") %>%
    owmr_as_tibble()) %>% names()

res[, 1:6]

# ... by city id
(beirut <- search_city_list("Beirut")) %>%
  as.list()

get_current(beirut$id, units = "metric") %>%
  owmr_as_tibble() %>% .[, 1:6]

res <- find_cities_by_geo_point(
  lat = beirut$lat,
  lon = beirut$lon,
  cnt = 5,
  units = "metric"
) %>% owmr_as_tibble()

idx <- c(names(res[1:6]), "name")
res[, idx]

forecast <- get_forecast("London", units = "metric") %>%
  owmr_as_tibble()

forecast[, 1:6]


# apply funcs to some columns
funcs <- list(
  temp = round,
  wind_speed = round
)
forecast %<>% parse_columns(funcs)

# do some templating ...
("{{dt_txt}}h {{temp}}°C, {{wind_speed}} m/s" %$$%
    forecast) %>% head(10)


################################################## Task_2 ##################################################

url= "https://www.rosebikes.de/fahrräder/rennrad"

#Wrap it into a function ----
  
get_bike_data <- function(url) {
  
  html_bike_category <- read_html(url)
  
  # Get the URLs
  bike_url_tbl  <- html_bike_category %>%
    html_nodes(css = ".catalog-category-bikes__title-text") %>%
    html_text()%>%
    enframe(name = "Number", value = "Bike.Name")
  
  # Get the descriptions
  bike_database_tbl<-bike_url_tbl%>% 
    mutate(price=html_bike_category%>%
             html_nodes(css =".catalog-category-bikes__price-title")%>%
             html_text())
}

bike_tableout<-get_bike_data(url)
bike_tableout
saveRDS(bike_tableout,"Data_Acquisition_Challenge.rds")