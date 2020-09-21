# defun_co2.R 
#################################################
# Description: This script is for running any 
# sequence of historical time-series data to make 
# a forecast for five values by a particular date.
# Assumes a cvs file with two columns (Date, Value) 
# with dates in reverse chronological order and in
# ISO-8601 format. Like so:
#
# 2019-08-06,1.73                                                                
# 2019-08-05,1.75                                                                
# 2019-08-02,1.86

#Clear memory and set string option for reading in data:
rm(list=ls())
gc()

  #################################################
  # Function
  defun_co2 <- function(time_path="./data/co2.csv", 
                         closing_date="2020-12-01", trading_days=7, 
                         bin1=410, bin2=412, 
                         bin3=414, bin4=416) {

  #################################################
  # Libraries
  #
  # Load libraries. If library X is not installed
  # you can install it with this command at the R prompt:
  # install.packages('X')

  # Determine how many days until end of question
  todays_date <- Sys.Date()
  closing_date <- as.Date(closing_date)
  remaining_weeks <- as.numeric(difftime(closing_date, todays_date, units = "weeks"))
  remaining_weeks <- round(remaining_weeks, digits=0)
  non_trading_days <- (7 - trading_days) * remaining_weeks
  day_difference <- as.numeric(difftime(closing_date, todays_date))
  remaining_days <- day_difference - non_trading_days 

  #################################################
  # Import & Parse
  # Point to time series data file and import it.
  #time_import <- read.csv("ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_trend_gl.txt", 
  #header=TRUE) 

  # If working with a downloaded file, comment out above and uncomment below
  time_import <- read.table("~/Downloads/co2_trend_gl.txt", header=TRUE)

  colnames(time_import) <- c("year", "month", "day", "value", "trend")
  
  # Puts these fields as numeric
  time_import$year <- as.numeric(time_import$year)
  time_import$month <- as.numeric(time_import$month)
  time_import$day <- as.numeric(time_import$day)
  
  # Combines %m/%d/%Y format into year
  time_import$year <- paste0(time_import$month, "/", time_import$day, "/", 
                         time_import$year)
  
  # Removes extra columns
  time_import <- time_import[,-c(2,3,5)]
  
  # Renames the column into the standard form for processing
  colnames(time_import) <- c("date", "value")
  
  # Imports reformed date as a date
  time_import$date <- as.Date(time_import$date, "%m/%d/%Y")
  
  # Makes the value into a vector
  time_import$value <- as.vector(as.numeric(time_import$value))
  
  # Places the data into reverse chronological
  time_import <- time_import[rev(order(time_import$date)),] 

  # Setting most recent value, assuming descending data
  current_value <- as.numeric(time_import[1,2])

  # Get the length of time_import$value and shorten it by remaining_days
  time_rows = length(time_import$value) - remaining_days

  # Create a dataframe
  time_calc <- NULL

  # Iterate through value and subtract the difference 
  # from the row remaining days away.
  for (i in 1:time_rows) {
    time_calc[i] <- time_import$value[i] - time_import$value[i+remaining_days]
  }

  # Adjusted against current values to match time_calc
  adj_bin1 <- bin1 - current_value
  adj_bin2 <- bin2 - current_value
  adj_bin3 <- bin3 - current_value 
  adj_bin4 <- bin4 - current_value 

  # Determine how many trading days fall in each question bin
  prob1 <- round(sum(time_calc<adj_bin1)/length(time_calc), digits = 3)
  prob2 <- round(sum(time_calc>=adj_bin1 & time_calc<=adj_bin2)/length(time_calc), digits = 3)
  prob3 <- round(sum(time_calc>adj_bin2 & time_calc<adj_bin3)/length(time_calc), digits = 3)
  prob4 <- round(sum(time_calc>=adj_bin3 & time_calc<=adj_bin4)/length(time_calc), digits = 3)
  prob5 <- round(sum(time_calc>adj_bin4)/length(time_calc), digits = 3)
  
  ###############################################
  # Print results
  return(cat(paste0("Probabilities for co2 on ", closing_date, ":\n",
                    prob1, ": Bin 1 - ", "<", bin1, "\n",
                    prob2, ": Bin 2 - ", bin1, " to <=", bin2, "\n", 
                    prob3, ": Bin 3 - ", bin2, "+ to <", bin3, "\n", 
                    prob4, ": Bin 4 - ", bin3, " to <=", bin4, "\n", 
                    prob5, ": Bin 5 - ", bin4, "+", "\n",
                    "Number of observations: ", time_rows)))
  }