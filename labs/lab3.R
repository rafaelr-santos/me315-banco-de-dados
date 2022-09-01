setwd("C:/Users/ra243464/Documents/repos/ME315/labs/")

library(tidyverse)
library(readr)

getStats <- function(input, pos){
    input %>%
        dplyr::filter(!startsWith(DESTINATION_AIRPORT, "1")) %>%
        drop_na() %>%
        dplyr::group_by(DESTINATION_AIRPORT) %>%
        dplyr::summarise(
            sumARRIVAL_DELAY = sum(ARRIVAL_DELAY),
            countDESTINATION_AIRPORT = n()
        )
}

airports <- read_csv(
    file = "airports.csv",    
    col_types = cols_only(
        IATA_CODE = col_character(),
        CITY = col_character(),
        STATE = col_character(),
        LATITUDE = col_double(),
        LONGITUDE = col_double()
    )
)

tst <- read_csv(
    file = "flights.csv",
    n_max = 10,
    col_types = cols_only(
        ARRIVAL_DELAY = col_double(),
        DESTINATION_AIRPORT = col_character()
    )
)

sufs <- read_csv_chunked(
    "flights.csv.zip",
    chunk_size = 1000000,
    callback = DataFrameCallback$new(getStats),
    col_types = cols_only(
        ARRIVAL_DELAY = col_double(),
        DESTINATION_AIRPORT = col_character()
    )
)

atrasos <- sufs %>%
dplyr::group_by(DESTINATION_AIRPORT) %>%
dplyr::summarise(ATRASO_MEDIO = sum(sumARRIVAL_DELAY)/sum(countDESTINATION_AIRPORT))


# Questão 2
atrasos
airports

flights <- atrasos %>% left_join(
    airports,
    by = c("DESTINATION_AIRPORT" = "IATA_CODE")
)

atrasos %>% anti_join(
    airports,
    by = c("DESTINATION_AIRPORT" = "IATA_CODE")
)

dplyr::group_by(flights, STATE) %>%
dplyr::summarise(n = n()) %>%
arrange(desc(n))

# Questão 3
library(leaflet)
flights %>%
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
        ~LONGITUDE,
        ~LATITUDE,
        radius = ~ATRASO_MEDIO,
        label=~paste(DESTINATION_AIRPORT, CITY, sep=" - "),
        clusterOptions = markerClusterOptions()
    )
