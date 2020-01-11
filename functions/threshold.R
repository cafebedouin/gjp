# defun_threshold.R 
#################################################
# Description: 

defun_threshold <- function(df,
                            prob_results_title="Testing",
                            closing_date="2020-11-01",
                            # freq options: daily, weekly, monthly, quarterly or yearly
                            freq="monthly",
                            # trading_days is only relevant for daily data
                            trading_days=5,
                            bin1=4) {
  
  #################################################
  # Test Data
  # Downloaded
  df <- read.csv("~/Downloads/UNRATE.csv", 
                 skip=0, header=TRUE)
  
  #################################################
  # Calculate time
  data_date <- as.Date(df[1,1])   
  closing_date <- as.Date(closing_date)
  remaining_weeks <- as.numeric(difftime(closing_date, data_date, units = "weeks"))
  remaining_weeks <- round(remaining_weeks, digits=0)
  remaining_time <- remaining_weeks
  
  # Convert weeks into period of interest
  if (freq == "daily") {
    non_trading_days <- (7 - trading_days) * remaining_weeks
    day_difference <- as.numeric(difftime(closing_date, data_date))
    remaining_time <- day_difference - non_trading_days 
  }  
  
  #  if (freq == "weekly") { remaining_time <- remaining_weeks }
  if (freq == "monthly") { 
    remaining_time <- round(sum(remaining_weeks / 4), digits = 0) }  
  if (freq == "quarterly") { 
    remaining_time <- round(sum(remaining_weeks / 12), digits = 0) }
  if (freq == "yearly") { 
    remaining_time <- round(sum(remaining_weeks / 52), digits = 0) }
  
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
  View(df)
  # Setting most recent value, assuming descending data
  current_value <- as.numeric(df[1,2])

  
  # Adjusted against current values to match prob_calc
  adj_bin1 <- bin1 - current_value
  
  # Get the length of df$value and shorten it by remaining_time
  prob_rows = length(df$value) - remaining_time
  
  # Create a dataframe
  prob_calc <- NULL
  
  # Change values to difference between value
  for (i in 1:prob_rows) {
    test <- NULL
    for (j in i:i+remaining_time) {
      test[j] <- if (j >= sum(df$value[j] + adj_bin1)) {1} else {0} 
    }
    prob_calc[i] <- if (j >= 0) {1} else {0}
  }
  View(prob_calc)
  # Empirically, how many trading days fall in each question bin?
  prob1 <- round(sum(prob_calc/length(prob_calc)), digits = 3)
  
  ###############################################
  # Print results
  return(cat(paste0(prob_results_title,  " for ", closing_date, ":\n", 
                    "There is a ", prob1, " historical probability ",
                    "from this data set that \n", 
                    "any given period of this length will hit the ",
                    "bin1 threshold.\n",
                    "Number of observations: ", prob_rows, "\n")))
}