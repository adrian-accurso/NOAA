library(testthat)
context("Test functions in the package")

file_name <- system.file("vignettes/earthquakesFile2.txt", package = "NOAA")
raw_data <- readr::read_delim(file_name, delim = "\t")

test_that("eq_clean_data returns a 'data.frame' object", {
  expect_is(eq_clean_data(raw_data), "data.frame")
})

test_that("eq_clean_data$DATE is of 'Date' type", {
  expect_is(eq_clean_data(raw_data)$DATE, "Date")
})

test_that("eq_clean_data returns numeric coordinates", {
  expect_is(eq_clean_data(raw_data)$LATITUDE, "numeric")
  expect_is(eq_clean_data(raw_data)$LONGITUDE, "numeric")
})

test_that("eq_location_clean returns a 'data.frame' object", {
  expect_is(eq_location_clean(raw_data), "data.frame")
})

test_that("geom_timeline returns a 'ggplot' object", {
  g <- raw_data %>% eq_clean_data() %>%
    dplyr::filter(COUNTRY %in% c("GREECE", "ITALY"), YEAR > 2000) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE,
                                 y = COUNTRY,
                                 color = as.numeric(TOTAL_DEATHS),
                                 size = as.numeric(EQ_PRIMARY)
    )) +
    geom_timeline()
  expect_is(g, "ggplot")
})

test_that("geom_timeline_label returns a 'ggplot' object", {
  g <- raw_data %>% eq_clean_data() %>%
    dplyr::filter(COUNTRY %in% c("GREECE", "ITALY"), YEAR > 2000) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE,
                                 y = COUNTRY,
                                 color = as.numeric(TOTAL_DEATHS),
                                 size = as.numeric(EQ_PRIMARY)
    )) +
    geom_timeline_label(ggplot2::aes(label = LOCATION_NAME))
  expect_is(g, "ggplot")
})

test_that("theme_timeline returns a 'ggplot' object", {
  g <- raw_data %>% eq_clean_data() %>%
    dplyr::filter(COUNTRY %in% c("GREECE", "ITALY"), YEAR > 2000) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE,
                                 y = COUNTRY,
                                 color = as.numeric(TOTAL_DEATHS),
                                 size = as.numeric(EQ_PRIMARY)
    )) +
    theme_timeline()
  expect_is(g, "ggplot")
})

test_that("eq_map returns a 'leaflet' object", {
  l <- raw_data %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "popup_text")
  expect_is(l, "leaflet")
})

test_that("eq_create_label returns a 'character' vector", {
 is.character(eq_create_label(eq_clean_data(raw_data)))
})

test_that("eq_create_label has the same length as input data frame", {
  expect_equal(length(eq_create_label(eq_clean_data(raw_data))), dim(eq_clean_data(raw_data))[1])
})
