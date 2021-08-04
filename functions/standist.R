# standist.R 
#################################################
# Description: Takes a two column data frame
# with columns date (chronological), value and 
# generates a projected mean increase for the 
# forecast period, adds that increase to last 
# value for the series, uses that projected mean 
# with standard deviation to generate a probability 
# table, assuming a standard distribution.
# 
# Use: Copy paste from question into spreadsheet.
# Save file into data folder. Refer to filename,
# projection period and bins in function call.

standist <- function(filename, # Org and Question No.
                     closing_date,   # How far ahead is the forecast based on data periods?
                     outlier,
                     bins) {
  
  # Prevent scientific notation
  options(scipen=999)
  
  # Load data
  df <- read.csv(paste0("./data/", filename, ".csv"), skip=0, header=TRUE)
  
  # Format for probability functions
  source("./functions/probform.R")
  df <- probform(df)
  
  View(df)
  
  # Run the remaining_time function
  source("./functions/remaining_time.R")
  remaining_time <- remaining_time(df,
                                   closing_date)
  
  # View(remaining_time)
  
  # Change percentage to whole number for multiplication
  if (mean(df$value) < 1) {
    df$value <- df$value * 100
  }
  
  # Drop outliers
  if (!is.null(outlier)) {
    df <- df[-c(outlier), ]
  }
  
  # Create a data frame
  percentages <- NULL
  
  # Creates a dataframe of change in value that matches forecast period
  for (i in 1:(length(df$value) - remaining_time)) {
    # Assumes newest at bottom
    percentages[i] <- log(df$value[i] / df$value[i+remaining_time])
  }
  
  View(percentages)
  
  # Current value
  current_value <- head(df$value, n=1)
  # View(current_value)
  
  # Adjusted to mean for similar periods over data available
  projected_mu <- current_value * exp(mean(percentages))
  # View(projected_mu)
  
  # Get the standard deviation of the data
  projected_sig <- sum(projected_mu * exp(sd(percentages)) - projected_mu)  
  # View(projected_sig)
  
  # Turns atomic vector bins into dataframe
  bins <- as.data.frame(bins)
  
  # Pnorm catches left side probabilities, so 0 to first bin
  # and pops that into our probability table
  bins$probs[1] <- pnorm(bins$bins[1], projected_mu, projected_sig) 
  
  # Skipping first bin, calculate probability of left of bin 
  # minus the probability of everything before previous bin
  for (i in 2:length(bins$bins)) {
    bins$probs[i] <- pnorm(bins$bins[i], projected_mu, projected_sig) - pnorm(bins$bins[i-1], projected_mu, projected_sig) 
  }
  
  # Round
  bins$probs <- round(bins$probs, digits = 3)
  cat(paste0("Projected mean: ", projected_mu, "\n",
             "Standard deviation: ", projected_sig, "\n"))
  
  # Tell me what the assumed mean was, standard deviation and probabilities
  return(bins)
}