fix_negative_incidence <- function(cases) {
  repeat{
    found_negative_cases <- FALSE
    for (i in 2:length(cases)) {
      if(cases[i] < 0){
        message(sprintf("Warning negative cases found on day %s correcting by applying those corrections to the previous day", i))
        cases[i-1] <- cases[i-1] + cases[i]
        cases[i] <- 0
        found_negative_cases <- TRUE
      }
    }
    if (!found_negative_cases) { break }
  }
  
  return(cases)
}