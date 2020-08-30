library(dplyr)
library(tidyr)
library(eurostat)

#' Total population within age bracket 
eurostat_demo <- get_eurostat(id="demo_pjan", 
                              cache = TRUE, 
                              time="num") %>%
  filter(geo=="AT") %>%
  filter(time==max(time)) %>%
  filter(sex=="T")

demo_age_group_total <- function(ages) {
  eurostat_filtered <- eurostat_demo %>%
    filter(age %in% sprintf("Y%d", ages))
  result <- sum(eurostat_filtered$values)
  
  if(0 %in% ages) {
    additional_pop <- eurostat_demo %>%
      filter(age=="Y_LT1") %>%
      select(values) %>%
      as.numeric()
    result <- result + additional_pop
  }
  
  if(100 %in% ages) {
    additional_pop <- eurostat_demo %>%
      filter(age=="Y_OPEN") %>%
      select(values) %>%
      as.numeric()
    result <- result + additional_pop
  }
  
  return(result)
}