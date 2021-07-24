# monte.R 
#################################################
# Description: This function takes a dataframe 
# with two columns (time, value). The first as.Date,
# in ISO-8601 format, in reverse chronological order, 
# and the second as a vector of numerical values.
# Example: 2019-08-06,1.73
#
# It then takes the remaining time, makes a vector of the log of the 
# quotient between current price divided by the price that was equal 
# to the  remaining time left before the question for all the data 
# available. It then calculates a log mean and standard deviation 
# to create a standard distribution, which is then used to randomly 
# choose values for the Monte Carlo simulation. These randomly chosen 
# values are then returned to normal values using exponent and 
# multiplied by the current price by the number of simulations
# required in the script.
#
# Example of output:
#
# Treasury Yields for 10 Year for 2021-09-16 forecast:
# ====================================================
# Prob. | Bins 
# ====================================================
# 0.202 | Bin 1 - <1
# 0.255 | Bin 2 - 1 to <=1.5
# 0.286 | Bin 3 - 1.5+ to <2
# 0.150 | Bin 4 - 2 to <=2.5
# 0.107 | Bin 5 - 2.5+
# ====================================================
# Number of Monte Carlo simulations: 10000

monte <- function(df,
                  # part before for in 1st line in example above
                  prob_results_title, # 
                  closing_date, # for question
                  trading_days=7, # per week for daily
                  freq="daily", # daily, weekly, monthly, quarterly, biyearly, yearly
                  # for five bins
                  hands=10000,
                  bins) {
  
  #################################################
  # Calculate time
  
  # Set todays_date
  todays_date <- Sys.Date()
  last_data_date <- as.Date(df[1,1])
  View(last_data_date)
  closing_date <- as.Date(closing_date)
  
  # Frequency: Interval for probability check, see:
  # https://www.datasciencemadesimple.com/get-difference-between-two-dates-in-r-by-days-weeks-months-and-years-r-2/
  
  # Adjust time differential to reflect the frequency of the df
  remaining_time <- if (freq == "weekly") {
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
    # Number of days - ((No. of days in week - trading days) * weeks) 
    as.numeric(difftime(closing_date, last_data_date)) -
      ((7 - trading_days) * 
         round(as.numeric(difftime(closing_date, last_data_date, units = "weeks")), digits = 0))
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
  
  # Removes NAs if they appear in values column
  df[!is.na(df[, 2]), ]
  
  # Setting most recent value, assuming decreasing order
  current_value <- as.numeric(df[1,2])
  
  View(df) # for testing
  
  # Get the length of df$value and shorten it by remaining_time
  deck_size <- length(df$value) - remaining_time
  
  # Create a dataframe
  deck <- NULL
  
  # Iterate through each value and subtract the difference 
  # from the next row, or period to create deck.
  for (i in 1:deck_size) {
    # Avoiding dividing by zero and negatives
    # since log normal needs positive values 
    if (df$value[i+remaining_time] <= 0) {
      deck[i] <- 0
    } else {
      # Logarithmic prices are always in a standard distribution
      deck[i] <- log(df$value[i] / df$value[i+remaining_time])
    }
  }
  
  deck[!is.finite(deck)] <- 0
  
  # Calculate standard deviation (sig) and mean of deck (mu)
  sig <- sd(deck)
  mu <- mean(deck)
  
  # Create data frame
  prob_calc <- NULL
  
  # Creates a standard distribution and random samples it 
  # the number of hands and multiples it by current value
  prob_calc <- current_value * exp(rnorm(hands, mean = mu, sd = sig))
  
  View(prob_calc) # for testing
  
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