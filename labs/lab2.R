setwd("C:/Users/ra243464/Documents/repos/ME315/labs/")

library(tidyverse)

amostra <- read_csv("flights.csv.zip", n_max = 100)

getStats <- function(input, pos){
    input %>%
    dplyr::filter(
        input,
        AIRLINE %in% c("AA", "DL", "UA", "US")
    ) %>%
    drop_na(AIRLINE, DAY, MONTH, YEAR, ARRIVAL_DELAY) %>%
    dplyr::group_by(DAY, MONTH, YEAR, AIRLINE) %>%
    dplyr::summarise(nAp = sum(ARRIVAL_DELAY > 10), nTp = n())
}


sufs <- read_csv_chunked(
    "flights.csv.zip",
    chunk_size = 100000,
    callback = DataFrameCallback$new(getStats),
    col_types = cols_only(
        AIRLINE = "c",
        DAY = "i",
        MONTH = "i",
        YEAR = "i",
        ARRIVAL_DELAY = "i"
    )
)
