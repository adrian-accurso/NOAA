#' Cleans raw data from NOAA
#'
#' \code{eq_clean_data} cleans date, latitude and longitude, and location name
#' from NOAA raw data
#'
#' @param raw_data A data frame with raw data obtained from NOAA website (see below)
#'
#' @return A data frame with cleaned date, latitude, longitude and location
#' columns
#'
#' @details The function takes raw data from NOAA website as an input
#' \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}. It adds
#' a column DATE with cleaned date (Date format), transforms LATITUDE and
#' LONGITUDE columns as numeric objects and transforms LOCATION_NAME by removing
#' the country and transforming to title case.
#'
#' @import dplyr
#' @importFrom lubridate years
#'
#' @examples
#' \dontrun{
#' data <- readr::read_delim("earthquakes.txt", delim = "\t")
#' clean_data <- eq_clean_data(data)
#' }
#'
#' @export
eq_clean_data <- function(raw_data){

  names(raw_data) <- toupper(names(raw_data))
  names(raw_data) <- sub(" ","_",names(raw_data))

  clean_data <- eq_location_clean(raw_data)

  dates <- apply(clean_data, 1,
                 function(row) ifelse(as.numeric(row["YEAR"]) < 0,
                                      as.Date(paste(abs(as.numeric(row["YEAR"])),
                                                    ifelse(is.na(row["MONTH"]),"01", row["MONTH"]),
                                                    ifelse(is.na(row["DAY"]), "01", row["DAY"]), sep = "-")) - lubridate::years(abs(as.numeric(row["YEAR"]))*2),
                                      as.Date(paste(row["YEAR"],
                                                    ifelse(is.na(row["MONTH"]),"01", row["MONTH"]),
                                                    ifelse(is.na(row["DAY"]), "01", row["DAY"]), sep = "-"))))

  clean_data$DATE <- as.Date(dates,format = "%Y-%m-%d", origin = "1970-01-01", tz = "GMT")
  clean_data$LATITUDE <- as.numeric(clean_data$LATITUDE)
  clean_data$LONGITUDE <- as.numeric(clean_data$LONGITUDE)

  return(clean_data)
}

#' Cleans earthquake location data
#'
#' @param raw_data A data frame with raw data obtained from NOAA website
#'
#' @return A data frame with cleaned LOCATION_NAME column
#'
#' @details This function transforms NOAA raw data frame's LOCATION_NAME column by
#' trimming the country name (if applicable) and converting to title case
#'
#' @note The function is not exported
#'
#' @examples
#' \dontrun{
#' raw_data <- readr::read_delim("earthquakes.txt", delim = "\t")
#' clean_data <- eq_location_clean(raw_data)
#' }
#'
eq_location_clean <- function(raw_data){

  raw_data$LOCATION_NAME <- sapply(raw_data$LOCATION_NAME,
                                   function(loc) {
                                     x <- gsub(".*:  ", "", loc)
                                     s <- strsplit(tolower(x), " ")[[1]]
                                     paste(toupper(substring(s, 1,1)), substring(s, 2),
                                           sep="", collapse=" ") })

  return(raw_data)
}
