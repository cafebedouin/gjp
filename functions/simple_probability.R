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
  
  # Set todays_date
  todays_date <- Sys.Date()

  # Run the remaining_time function
  source("./functions/remaining_time.R")
  remaining_time <- remaining_time(df,
                                   closing_date,
                                   trading_days,
                                   freq)
  
  # Format for probability functions
  source("./functions/probform.R")
  df <- probform(df)

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
  
  # View(prob_calc)

  # Sort probabilities into bins 
  source("./functions/sortbins.R")
  bins <- sortbins(bins,
                   prob_calc)
  
  # print(bins)
  return(bins)
}