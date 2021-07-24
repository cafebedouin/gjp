# monte.R 
#################################################
# Description: Calculates remaining_time

remaining_time <- function(df,
                           closing_date,
                           trading_days,
                           freq="daily") {
  
  # Set todays_date
  last_data_date <- as.Date(df[1,1])
  closing_date <- as.Date(closing_date)
  
  # Frequency: Interval for probability check, see:
  # https://www.datasciencemadesimple.com/get-difference-between-two-dates-in-r-by-days-weeks-months-and-years-r-2/
  
  # Adjust time differential to reflect the frequency of the df
  remaining_time <- if (freq == "daily") {
    as.numeric(difftime(closing_date, last_data_date)) -
      ((7 - trading_days) * 
         round(as.numeric(difftime(closing_date, last_data_date, units = "weeks")), digits = 0))
  } else if (freq == "weekly") {
    round(as.numeric(difftime(closing_date, last_data_date, units = "weeks")), digits = 0)
  } else if (freq == "monthly") {
    round(as.numeric(difftime(closing_date, last_data_date, units = "days")/(365.25/12)), digits = 0)
  } else if (freq == "quarterly") {
    round(as.numeric(difftime(closing_date, last_data_date, units = "days")/(365.25/4)), digits = 0)
  } else if (freq == "biyearly") {
    round(as.numeric(difftime(closing_date, last_data_date, units = "days")/365.25/2), digits = 0)
  } else if (freq == "yearly") {
    round(as.numeric(difftime(closing_date, last_data_date, units = "days")/365.25), digits = 0)
  } else { # defaults to daily
  return(paste0("Error in remaining_time"))
  }
  return(remaining_time)
}