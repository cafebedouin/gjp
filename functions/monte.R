# monte.R 
#################################################
# Description: This function takes a dataframe 
# with two columns (time, value). The first as.Date,
# in ISO-8601 format, in reverse chronological order, 
# and the second as a vector of numerical values.
# Example: 2019-08-06,1.73
#
# It then takes the remaining time, makes a vector of the quotient 
# between current price divided by the price that was equal to the 
# remaining time left before the question for all the data available.
# It then calculates a mean and standard deviation to create a 
# standard distribution, which is then used to randomly choose values
# for the Monte Carlo simulation. These randomly chosen values
# are then multiplied by the current price by the number of simulations
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
                  bin1,
                  bin2,
                  bin3,
                  bin4) {

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
  deck_size <- length(df$value) - remaining_time

  # Create a dataframe
  deck <- NULL

  # Iterate through each value and subtract the difference 
  # from the next row, or period to create deck.
  for (i in 1:deck_size) {
    # Avoiding dividing by zero and negatives
    if (df$value[i+remaining_time] <= 0) {
      deck[i] <- 1
    } else {
      deck[i] <- df$value[i] / df$value[i+remaining_time]
    }
  }

  # Calculate standard deviation and mean of deck
  deck_stdev <- sd(deck)
  deck_mean <- mean(deck)
  
  # Create data frame
  prob_calc <- NULL
  
  # Draw from deck the remaining periods of question,
  # add the percentages together, subtract remaining time
  # minus 1 to index to 1, multiple result by current value 
  # and save the result to generate a probability table.
  prob_calc <- current_value * rnorm(hands, mean = deck_mean, sd = deck_stdev)
  
  # Empirically, how many trading days fall in each question bin?
  prob1 <- round(sum(prob_calc<bin1)/length(prob_calc), digits = 3)
  prob2 <- round(sum(prob_calc>=bin1 & prob_calc<=bin2)/length(prob_calc), digits = 3)
  prob3 <- round(sum(prob_calc>bin2 & prob_calc<bin3)/length(prob_calc), digits = 3)
  prob4 <- round(sum(prob_calc>=bin3 & prob_calc<=bin4)/length(prob_calc), digits = 3)
  prob5 <- round(sum(prob_calc>bin4)/length(prob_calc), digits = 3)
  
  ###############################################
  # Print results
  return(cat(paste0(prob_results_title,  " for ", closing_date, " forecast:\n",
                    "====================================================\n",
                    "Prob. | Bins \n", 
                    "====================================================\n",
                    prob1, " | Bin 1 - ", "<", bin1, "\n",
                    prob2, " | Bin 2 - ", bin1, " to <=", bin2, "\n", 
                    prob3, " | Bin 3 - ", bin2, "+ to <", bin3, "\n", 
                    prob4, " | Bin 4 - ", bin3, " to <=", bin4, "\n", 
                    prob5, " | Bin 5 - ", bin4, "+", "\n",
                    "====================================================\n",
                    "Number of Monte Carlo simulations: ", hands, "\n")))
}