# monte.R 
#################################################
# Description: Calculates remaining_time

remaining_time <- function(df,
                           closing_date) {

  # Set variables, assumes reverse chronological order for dates
  last_data_date <- as.Date(df[1,1])
  first_data_date <- as.Date(tail(df$date, n=1))
  closing_date <- as.Date(closing_date)
  
  # Calculate
  data_interval <- sum(as.numeric(difftime(last_data_date, first_data_date)) 
                       / length(df$value))
  remaining_time <- sum(as.numeric(difftime(closing_date, last_data_date))
                        / data_interval)
  
  # Return an integer
  remaining_time <- round(remaining_time, digits = 0)
  
  return(remaining_time)
}
