# csv-oil.R 
#################################################
# Description: Imports csv file and has defaults
# for removing columns, rows, filtering on 
# content and putting the last date/value pairs
# in the appropriate data types.

# Replace defaults in function to desired, or 
# call the function from console
csv <- function(begin_date, # For analysis, not question
                closing_date,
                bins,
                probability_type="monte") {
  
  #################################################
  # Preliminaries
  
  # Preventing scientific notation in graphs
  options(scipen=999)

  # Set todays_date
  todays_date <- Sys.Date()
  
  # Set date for ten years ago
  ten_years <- todays_date - 3652
  
  #################################################
  # Load libraries. If library X is not installed
  # you can install it with this command at the R prompt:
  # install.packages('X')
  library(data.table)
  library(dplyr)
  
  #################################################
  # Import, organize and output csv data
  df <- read.csv(paste0("./data/oil.csv"), 
                 skip=0, header=TRUE)

  # Drop columns
  # df <- df[ -c(3:20) ]
  
  # Drop rows
  # df <- df[ -c(1), ]
  
  # Names the columns
  colnames(df) <- c("date", "value")
  
  # Filter out days with value of "."
  df <- filter(df, value != ".") 
  df %>% is.na()
  
  # Turn date into a date
  # df$date <- paste0(df$date, "-01")

  # Making sure data types are in the correct format
  df$date <- as.Date(df$date)
  df$value <- as.character(df$value) # for converting factors
  df$value <- as.numeric(df$value)
  
  # Reverse dates 
  df <- df[rev(order(df$date)),] 
  
  # Filter 
  df <- filter(df, date >= begin_date)
  
  # Visually check columns, data types and data
  glimpse(df)
  
  # Writes csv file
  #write.csv(df, file=paste0("./output/stub-", 
  #                          todays_date,
  #                          ".csv")) 
  
  #################################################
  # Call desired forecasting functions
  
  # Simple walk through historical values to generate probabilities
  if (probability_type == "simple") {
    source("./functions/simple_probability.R")
    return(simple_probability(df, closing_date, bins)) }

  # Simple Monte Carlo using historical period changes 
  # and number of hands to generate probabilities
  if (probability_type == "monte") {
    source("./functions/monte.R")
    return(monte(df, closing_date, hands=10000, bins)) }
  
  # Simple Monte Carlo using historical period changes 
  # and number of hands to generate probabilities
  if (probability_type == "standist") {
    source("./functions/standist.R")
    return(standish(df, closing_date, hands=10000, bins)) }
}