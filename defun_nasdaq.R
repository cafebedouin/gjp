# defun_nasdaq.R 
#################################################
# Description: Pulls NASDAQ data by ticker and 
# from the NASDAQ website, allows you to limit the 
# data set, then provides a probability base line
# against <=5 bins or 4 price points using current
# price and walk through the outcomes in set.

# Replace defaults in function to desired, or 
# call the function from console
defun_nasdaq <- function(ticker="TSLA", 
                       begin_date="2014-01-01", # For analysis, not question
                       closing_date="2020-12-31", 
                       trading_days=5, 
                       bin1=250, 
                       bin2=300, 
                       bin3=350, 
                       bin4=400) {
  
  #################################################
  # Preliminaries
  
  # Define basepath and set working directory:
  basepath = "~/Documents/programs/R/forecasting"
  setwd(basepath)
  
  # Preventing scientific notation in graphs
  options(scipen=999)
  
  # Set todays_date
  todays_date <- Sys.Date()
  
  # Create url to access data
  nurl = paste0("https://www.nasdaq.com/api/v1/historical/",
                ticker, "/stocks/", begin_date, "/", todays_date)
  
  #################################################
  # Load libraries. If library X is not installed
  # you can install it with this command at the R prompt:
  # install.packages('X')
  library(data.table)

  #################################################
  # Import, organize and output csv data
  
  # Live import
  nasdaq <- read.csv(nurl, skip=1, header=TRUE)
  
  # Downloaded
  # nasdaq <- read.csv("/home/scott/Downloads/HistoricalQuotes.csv", 
  #                   skip=1, header=TRUE)

  # Selects date and closing value
  nasdaq = nasdaq[, -c(3:6)]
  
  # Names the columns
  colnames(nasdaq) <- c("date", "value")
  
  # Translates the NASDAQ date into one R likes
  nasdaq$date <- as.Date(nasdaq$date, "%m/%d/%Y")
  
  # Removes the dollar signs from values
  nasdaq$value <- as.numeric(gsub("[\\$,]", "", nasdaq$value))
  
  # All of this is to determine how many days until end of question
  closing_date <- as.Date(closing_date)
  remaining_weeks <- as.numeric(difftime(closing_date, todays_date, units = "weeks"))
  remaining_weeks <- round(remaining_weeks, digits=0)
  non_trading_days <- (7 - trading_days) * remaining_weeks
  day_difference <- as.numeric(difftime(closing_date, todays_date))
  remaining_days <- day_difference - non_trading_days 
  
  # Making sure data types are in the correct format
  nasdaq$date <- as.Date(nasdaq$date)
  nasdaq$value <- as.vector(nasdaq$value)
  
  View(nasdaq)
  
  # Setting most recent value, assuming descending data
  current_value <- as.numeric(nasdaq[1,2])
  
  # Get the length of nasdaq$value and shorten it by remaining_days
  time_rows = length(nasdaq$value) - remaining_days
  
  # Create a dataframe
  time_calc <- NULL
  
  # Iterate through value and subtract the difference 
  # from the row remaining days away.
  for (i in 1:time_rows) {
    time_calc[i] <- nasdaq$value[i] - nasdaq$value[i+remaining_days]
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
  return(cat(paste0("Probabilities for closing price of ", ticker,  
                    " on ", closing_date, ":\n",
                    prob1, ": Bin 1 - ", "<", bin1, "\n",
                    prob2, ": Bin 2 - ", bin1, " to <=", bin2, "\n", 
                    prob3, ": Bin 3 - ", bin2, "+ to <", bin3, "\n", 
                    prob4, ": Bin 4 - ", bin3, " to <=", bin4, "\n", 
                    prob5, ": Bin 5 - ", bin4, "+", "\n",
                    "Number of observations: ", time_rows)))
}