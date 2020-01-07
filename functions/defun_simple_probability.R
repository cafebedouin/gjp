# defun_simple_probability.R 
#################################################
# Description: This function takes a dataframe 
# with two columns (time, value). The first as.Date,
# in ISO-8601 format, in reverse chronological order, 
# and the second as a vector of numerical values.
# Example: 2019-08-06,1.73
#
# It then takes the difference between today's price
# and the historical values of the same duration and
# creates a probability table based on defined bins.
#
# Example of desired output:
# Y30Y Treasury Yield Probabilities for 2020-12-31:
# 0.001: Bin 1 - <0
# 0.365: Bin 2 - 0 to <=2
# 0.303: Bin 3 - 2+ to <2.5
# 0.247: Bin 4 - 2.5 to <=3
# 0.084: Bin 5 - 3+
# Number of observations: 1877

defun_simple_probability <- function(df,
                                     prob_results_title,
                                     closing_date,
  # freq options: daily, weekly, monthly, quarterly or yearly
                                     freq="daily",
  # trading_days is only relevant for daily data
                                     trading_days=7,
                                     bin1,
                                     bin2,
                                     bin3,
                                     bin4) {
  
  #################################################
  # Calculate time
  todays_date <- Sys.Date()
  closing_date <- as.Date(closing_date)
  remaining_weeks <- as.numeric(difftime(closing_date, todays_date, units = "weeks"))
  remaining_weeks <- round(remaining_weeks, digits=0)
  remaining_time <- remaining_weeks
  
  # Convert weeks into period of interest
  if (freq == "daily") {
    non_trading_days <- (7 - trading_days) * remaining_weeks
    day_difference <- as.numeric(difftime(closing_date, todays_date))
    remaining_time <- day_difference - non_trading_days 
  }  

#  if (freq == "weekly") { remaining_time <- remaining_weeks }
  if (freq == "monthly") { remaining_time <- remaining_weeks / 4 }  
  if (freq == "quarterly") { remaining_time <- remaining_weeks / 13 }
  if (freq == "yearly") { remaining_time <- remaining_weeks / 52 }
    
  #################################################
  # Check formating
  
  # Changing column name to required values
  colnames(df) <- c("date", "value")

  # Clips off time and imports value as number
  df$date <- as.Date(df$date)
  df$value <- as.numeric(df$value) 

  # Puts into date decreasing order
  df <- df[rev(order(as.Date(df$date), na.last = NA)),]

  # Putting into vectors format
  df$value <- as.vector(df$value) 
  df$date <- as.Date(df$date)

  # Setting most recent value, assuming descending data
  current_value <- as.numeric(df[1,2])

  # Get the length of df$value and shorten it by remaining_time
  prob_rows = length(df$value) - remaining_time

  # Create a dataframe
  prob_calc <- NULL

  # Iterate through value and subtract the difference 
  # from the row remaining days away.
  for (i in 1:prob_rows) {
    prob_calc[i] <- df$value[i] - df$value[i+remaining_time]
  }

  # Adjusted against current values to match prob_calc
  adj_bin1 <- bin1 - current_value
  adj_bin2 <- bin2 - current_value
  adj_bin3 <- bin3 - current_value 
  adj_bin4 <- bin4 - current_value 

  # Empirically, how many trading days fall in each question bin?
  prob1 <- round(sum(prob_calc<adj_bin1)/length(prob_calc), digits = 3)
  prob2 <- round(sum(prob_calc>=adj_bin1 & prob_calc<=adj_bin2)/length(prob_calc), digits = 3)
  prob3 <- round(sum(prob_calc>adj_bin2 & prob_calc<adj_bin3)/length(prob_calc), digits = 3)
  prob4 <- round(sum(prob_calc>=adj_bin3 & prob_calc<=adj_bin4)/length(prob_calc), digits = 3)
  prob5 <- round(sum(prob_calc>adj_bin4)/length(prob_calc), digits = 3)

  ###############################################
  # Print results
  return(cat(paste0(prob_results_title,  " for ", closing_date, ":\n", 
                  prob1, ": Bin 1 - ", "<", bin1, "\n",
                  prob2, ": Bin 2 - ", bin1, " to <=", bin2, "\n", 
                  prob3, ": Bin 3 - ", bin2, "+ to <", bin3, "\n", 
                  prob4, ": Bin 4 - ", bin3, " to <=", bin4, "\n", 
                  prob5, ": Bin 5 - ", bin4, "+", "\n",
                  "Number of observations: ", prob_rows, "\n")))
}