library(here)
library(dplyr)
library(lubridate)

convert_age_stratified_time_series <- function(data_folder = here("external_data/austria-covid-data/data"),
                                               output = here("data/age-stratified-incidence-austria.csv")) {
  
  if(!dir.exists(data_folder)) {
    warning("The external_data folder with the data lake does not exist. Skipping data update!")
    return()
  }
  
  #' Converts files from data.gv.at folder archives into usable age stratified time-series
  datalist = list()
  i = 1
  for (directory in list.files(data_folder, no..=TRUE)) {
    tryCatch({
      data <- read.csv(
        sprintf("%s/%s/Altersverteilung.csv", data_folder, directory)
        , sep = ";")
      datalist[[i]] <- data
      i <- i + 1
    },
    error=function(cond) {
      message(sprintf("One import failed for directory %s:\n%s\nContinuing with other imports", directory, cond))
      # Choose a return value in case of error
      return(NA)
    })
  }
  
  age_stratified_incidence = do.call(rbind, datalist) %>%
    mutate(Timestamp=parse_date_time(Timestamp, "Ymd HMS")) %>%
    mutate(date=as.Date(Timestamp)) %>%
    arrange(desc(Timestamp)) %>%
    distinct(date, Altersgruppe, .keep_all = TRUE) %>%
    select(Altersgruppe, Anzahl, date) %>%
    arrange(date) %>%
    group_by(Altersgruppe) %>%
    mutate(incidence=Anzahl-lag(Anzahl, 1)) %>%
    filter(date>min(date)) %>% 
    rename("age_group"=Altersgruppe, "cumulative"=Anzahl)
  
  
  write.csv(age_stratified_incidence, output)
}
