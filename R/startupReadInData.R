#'Startup
# setwd("~/Desktop/Coursera Capstone Project")
library(magrittr)
# library(tidyverse)
install.packages('terra', repos='https://rspatial.r-universe.dev')
library(terra)
library(leaflet)
data <- readr::read_delim("earthquakesFile2.txt", delim = "\t")


data %>% eq_clean_data() %>%
    filter(COUNTRY %in% c("MEXICO", "GUATEMALA"), YEAR > 20) %>%
    ggplot(aes(x = DATE,
               y = COUNTRY,
               color = as.numeric(TOTAL_DEATHS),
               size = as.numeric(EQ_PRIMARY)
    )) +
    geom_timeline() +
    theme_timeline() +
    labs(size = "Richter scale value", color = "# deaths")


readr::read_delim("earthquakesFile2.txt", delim = "\t") %>%
  eq_clean_data() %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 400) %>%
  eq_map(annot_col = "DATE")



readr::read_delim("earthquakesFile2.txt", delim = "\t") %>%
  eq_clean_data() %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = "popup_text")
