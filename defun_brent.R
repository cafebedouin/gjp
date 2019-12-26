# defun_brent.R 
#################################################
# Description: Pulls Brent Crude Oil pricing data 
# from the EIA website, allows you to limit the 
# data set, then provides a probability base line
# against <=5 bins or 4 price points using current
# price and walk through the outcomes in set.
# Set the parameters in the function, and then
# call the function as part of an update.

#Clear memory and set string option for reading in data:
rm(list=ls())
gc()

#################################################
# Function
defun_brent <- function(closing_date="2020-04-01",
                        start_date="2014-01-01",
                        trading_days=5, 
                        bin1=63, 
                        bin2=69, 
                        bin3=200, 
                        bin4=300) {
  
  #################################################
  # Libraries
  #
  # Load libraries. If library X is not installed
  # you can install it with this command at the R prompt:
  # install.packages('X')
  library(readxl)
  library(xml2)
  library(lubridate)
  library(dplyr)
  library(httr)
  
  # Determine how many days until end of question
  todays_date <- Sys.Date()
  start_date <- as.Date(start_date)
  closing_date <- as.Date(closing_date)
  remaining_weeks <- as.numeric(difftime(closing_date, todays_date, units = "weeks"))
  remaining_weeks <- round(remaining_weeks, digits=0)
  non_trading_days <- (7 - trading_days) * remaining_weeks
  day_difference <- as.numeric(difftime(closing_date, todays_date))
  remaining_days <- day_difference - non_trading_days 
  
  #################################################
  # Import & Parse
  # Point to time series data file and import it.
  
  # Necessary kludge because read_excel doesn't know about urls yet
  kludge <- tempfile(fileext = ".xls")
  GET(url = "http://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls",
      write_disk(kludge), progress())
  
  # This should work whenever read_excel learns about the web
  # time_import <- read_excel("http://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls", 
  #                           sheet = "Data 1")
  #
  # Uncomment for testing against downloaded file.
  # time_import <- read_excel("~/Downloads/RBRTEd.xls", sheet = "Data 1")
  
  time_import <- read_excel(kludge, sheet = "Data 1")
  time_import <- time_import[-c(1,2),]
  colnames(time_import) <- c("date", "value")

  # Cleaning up import
  time_import$date <- as.numeric(time_import$date)
  time_import$date <- as.Date(time_import$date, origin = "1899-12-30")
  time_import$value <- as.vector(as.numeric(time_import$value))
  time_import <- time_import[rev(order(time_import$date)),]   

  # Filter out old data
  time_import <- filter(time_import, date >= start_date)
  
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

  # Empirically, how many trading days fall in each question bin?
  prob1 <- round(sum(time_calc<adj_bin1)/length(time_calc), digits = 3)
  prob2 <- round(sum(time_calc>=adj_bin1 & time_calc<=adj_bin2)/length(time_calc), digits = 3)
  prob3 <- round(sum(time_calc>adj_bin2 & time_calc<adj_bin3)/length(time_calc), digits = 3)
  prob4 <- round(sum(time_calc>=adj_bin3 & time_calc<=adj_bin4)/length(time_calc), digits = 3)
  prob5 <- round(sum(time_calc>adj_bin4)/length(time_calc), digits = 3)

  ###############################################
  # Print results
  return(cat(paste0("Brent Crude Oil Probabilities for ", closing_date, ":\n", 
                    prob1, ": Bin 1 - ", "<", bin1, "\n",
                    prob2, ": Bin 2 - ", bin1, " to <=", bin2, "\n", 
                    prob3, ": Bin 3 - ", bin2, "+ to <", bin3, "\n", 
                    prob4, ": Bin 4 - ", bin3, " to <=", bin4, "\n", 
                    prob5, ": Bin 5 - ", bin4, "+", "\n",
                    "Number of observations: ", time_rows)))
}