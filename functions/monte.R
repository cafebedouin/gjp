# monte.R 
#################################################
# Description: This function takes a dataframe 
# with two columns (time, value). The first as.Date,
# in ISO-8601 format, in reverse chronological order, 
# and the second as a vector of numerical values.
# Example: 2019-08-06,1.73
#
# It then makes a vector of the difference between 
# a specific point in time and the period before to create 
# a list of daily change in values. Then, it takes the number
# of periods left in the question and randomly picks
# values for the number of periods left in the question,
# and sums the values to determine possible changes
# that could occur over that period based on historical
# values. It then creates probabilities from the
# number of simulations run.
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
  deck_size <- length(df$value) - 1

  # Create a dataframe
  deck <- NULL

  # Iterate through each value and subtract the difference 
  # from the next row, or period to create deck.
  for (i in 1:deck_size) {
    deck[i] <- df$value[i] - df$value[i+1]
  }
  
  prob_calc <- NULL
  
  # Draw from deck the remaining periods of question,
  # add them together, add them to current value and
  # save the result to generate a probability table.
  for(i in 1:hands){
    prob_calc[i] <- current_value + sum(sample(deck, remaining_time, replace = FALSE))
  }
  
  View(prob_calc)
  
  # Display summary stats
  summary(prob_calc)

  # Empirically, how many trading days fall in each question bin?
  prob1 <- round(sum(prob_calc<bin1)/length(prob_calc), digits = 3)
  prob2 <- round(sum(prob_calc>=bin1 & prob_calc<=bin2)/length(prob_calc), digits = 3)
  prob3 <- round(sum(prob_calc>bin2 & prob_calc<bin3)/length(prob_calc), digits = 3)
  prob4 <- round(sum(prob_calc>=bin3 & prob_calc<=bin4)/length(prob_calc), digits = 3)
  prob5 <- round(sum(prob_calc>bin4)/length(prob_calc), digits = 3)

  # Calculate Briers
  brier1 <- round(mean((1 - prob1)^2 + (0 - prob2)^2 + (0 - prob3)^2 + (0 - prob4)^2 + (0 - prob5)^2), digits = 3)
  brier2 <- round(mean((0 - prob1)^2 + (1 - prob2)^2 + (0 - prob3)^2 + (0 - prob4)^2 + (0 - prob5)^2), digits = 3)
  brier3 <- round(mean((0 - prob1)^2 + (0 - prob2)^2 + (1 - prob3)^2 + (0 - prob4)^2 + (0 - prob5)^2), digits = 3)
  brier4 <- round(mean((0 - prob1)^2 + (0 - prob2)^2 + (0 - prob3)^2 + (1 - prob4)^2 + (0 - prob5)^2), digits = 3)
  brier5 <- round(mean((0 - prob1)^2 + (0 - prob2)^2 + (0 - prob3)^2 + (0 - prob4)^2 + (1 - prob5)^2), digits = 3)
  
  ###############################################
  # Print results
  return(cat(paste0(prob_results_title,  " for ", closing_date, " forecast:\n",
                    "====================================================\n",
                    "Prob. | Brier | Bins \n", 
                    "====================================================\n",
                    prob1, " | ", brier1, " | Bin 1 - ", "<", bin1, "\n",
                    prob2, " | ", brier2, " | Bin 2 - ", bin1, " to <=", bin2, "\n", 
                    prob3, " | ", brier3, " | Bin 3 - ", bin2, "+ to <", bin3, "\n", 
                    prob4, " | ", brier4, " | Bin 4 - ", bin3, " to <=", bin4, "\n", 
                    prob5, " | ", brier5, " | Bin 5 - ", bin4, "+", "\n",
                    "====================================================\n",
                    "Number of Monte Carlo simulations: ", hands, "\n",
                    "Note: Brier scores assume 5 bins and \n",
                  "      the line it is on is correct.\n")))
}