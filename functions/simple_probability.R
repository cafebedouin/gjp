# simple_probability.R 
#################################################
# Description: This function takes a dataframe 
# with two columns (time, value). The first as.Date,
# in ISO-8601 format, in reverse chronological order, 
# and the second as a vector of numerical values.
# Example: 2019-08-06,1.73
#
# It then takes the difference between today's values
# and the historical values of the same duration - whether
# daily, weekly, monthly, quarterly or yearly - and
# creates a probability table based on defined bins.
#
# Example of output:
#
# Treasury Yields for 30 Year for 2021-09-16 forecast:
# ====================================================
# Prob. | Brier | Bins 
# ====================================================
# 0.205 | 0.810 | Bin 1 - <1
# 0.278 | 0.664 | Bin 2 - 1 to <=1.5
# 0.210 | 0.800 | Bin 3 - 1.5+ to <2
# 0.222 | 0.776 | Bin 4 - 2 to <=2.5
# 0.084 | 1.052 | Bin 5 - 2.5+
# ====================================================
# Number of observations: 2375

simple_probability <- function(df,
                               # part before for in 1st line in example above
                               prob_results_title, # 
                               closing_date, # for question
                               trading_days=7, # per week
                               freq="daily", # daily, weekly, monthly, quarterly, biyearly, yearly
                               # for five bins
                               bins) {

  #################################################
  # Calculate time
  todays_date <- Sys.Date()
  closing_date <- as.Date(closing_date)
  
  # Frequency: Interval for probability check, see:
  # https://www.datasciencemadesimple.com/get-difference-between-two-dates-in-r-by-days-weeks-months-and-years-r-2/

  # Adjust time differential to reflect the frequency of the df
  remaining_time <- if (freq == "weekly") {
    round(as.numeric(difftime(closing_date, todays_date, units = "weeks")), digits = 0)
  } else if (freq == "monthly") {
    round(as.numeric(difftime(closing_date, todays_date, units = "days")/(365.25/12)), digits = 0)
  } else if (freq == "quarterly") {
    round(as.numeric(difftime(closing_date, todays_date, units = "days")/(365.25/4)), digits = 0)
  } else if (freq == "biyearly") {
    round(as.numeric(difftime(closing_date, todays_date, units = "days")/365.25/2), digits = 0)
  } else if (freq == "yearly") {
    round(as.numeric(difftime(closing_date, todays_date, units = "days")/365.25), digits = 0)
  } else { # defaults to daily
    # Number of days - ((No. of days in week - trading days) * weeks) 
    as.numeric(difftime(closing_date, todays_date)) -
    ((7 - trading_days) * 
      round(as.numeric(difftime(closing_date, todays_date, units = "weeks")), digits = 0))
  }
  
  #################################################
  # Check formating
  
  # Changing column name to required values
  colnames(df) <- c("date", "value")

  # Clips off time and imports value as number
  df$date <- as.Date(df$date)
  df$value <- as.numeric(df$value) 

  # Puts into date decreasing order
  df <- df[rev(order(as.Date(df$date), na.last = NA)),]

  # Setting most recent value, assuming decreasing order
  current_value <- as.numeric(df[1,2])

  # Get the length of df$value and shorten it by remaining_time
  prob_rows <- length(df$value) - remaining_time

  # Create a dataframe
  prob_calc <- NULL

  # Iterate through value and subtract the difference 
  # from the row remaining time in question away.
  for (i in 1:prob_rows) {
    prob_calc[i] <- current_value * (df$value[i] / df$value[i+remaining_time])
  }
  
  View(prob_calc)

  bins <- as.data.frame(bins)
  
  # Sort bins into lowest to highest order
  # bins <- bins[order(as.numeric(bins)),] 
  
  # Sort probabilities into bins
  for (i in 1:length(bins$bins)) {
    if (i == 1) {
      # Checks to see probabilities below lowest value
      bins$probs[i] <- round(sum(prob_calc<bins$bins[i])/length(prob_calc), digits = 3)
    } else if ((i %% 2) == 0) {
      bins$probs[i] <- round(sum(prob_calc>=bins$bins[i-1] & prob_calc<=bins$bins[i])/length(prob_calc), digits = 3)
    } else if ((i %% 2) != 0) {
      bins$probs[i] <- round(sum(prob_calc>bins$bins[i-1] & prob_calc<bins$bins[i])/length(prob_calc), digits = 3)
    }
  }
  
  print(bins)
  return(bins)
}