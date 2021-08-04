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

monte <- function(df,
                  closing_date, # for question
                  hands=10000,
                  bins) {
  
  # Set todays_date
  todays_date <- Sys.Date()
  
  # Format for probability functions
  source("./functions/probform.R")
  df <- probform(df)
  
  # Run the remaining_time function
  source("./functions/remaining_time.R")
  remaining_time <- remaining_time(df,
                                   closing_date)
  
  # Replaces negative values with zero
  df$value <- replace(df$value, df$value < 0,0) 
  
  # Setting most recent value, assuming decreasing order
  current_value <- as.numeric(df[1,2])
  
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
      deck[i+remaining_time] <- 1
    } else {
      # Logarithmic prices are always in a standard distribution
      deck[i] <- log(df$value[i] / df$value[i+remaining_time])
    }
  }
  
  deck[!is.finite(deck)] <- 0
  deck[is.na(deck)] <- 0
  
  # Calculate standard deviation (sig) and mean of deck (mu)
  sig <- sd(deck)
  mu <- mean(deck)
  
  # Create data frame
  prob_calc <- NULL
  
  # Creates a standard distribution and random samples it 
  # the number of hands and multiples it by current value
  prob_calc <- current_value * exp(rnorm(hands, mean = mu, sd = sig))
  
  # View(prob_calc) # for testing
  
  # Sort probabilities into bins
  source("./functions/sortbins.R")
  bins <- sortbins(bins,
                   prob_calc)
  
  # print(bins)
  return(bins)
}