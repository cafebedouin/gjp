# standist.R 
#################################################
# Description: Takes a two column data frame
# with columns date, quant and generates a projected
# mean increase for the forecast period, adds that 
# increase to last value for the series, uses that
# projected mean with standard deviation to generate
# a probability table, assuming a standard distribution.
# 
# Use: Copy paste from question into spreadsheet.
# Save file into data folder. Refer to filename,
# projection period and bins in function call.

standist <- function(filename, # Org and Question No.
                     forecast_periods,   # How far ahead is the forecast based on data periods?
                     outlier,
                     bins) {
  
  # Prevent scientific notation
  options(scipen=999)
  
  # Load data
  df <- read.csv(paste0("./data/", filename, ".csv"), skip=0, header=TRUE)

  # Drop outliers
  if (!is.null(outlier)) {
    df <- df[-c(outlier), ]
  }
  
  # Create a data frame
  period_inc <- NULL
  
  # Creates a dataframe of change in value that matches forecast period
  for (i in 1:(length(df$quant) - forecast_periods)) {
    period_inc[i] <- df$quant[i+forecast_periods] / df$quant[i]
  }
  
  View(period_inc)
  
  # Mean increase for similar periods over data available
  mean_inc <- mean(period_inc)
  
  # Get the standard deviation of the data and increase it in proportion
  df_sd <- sd(df$quant) * mean_inc
  
  # Creates a projected mean for the period being forecasted
  df_adj_mean <- df$quant[length(df$quant)] * mean_inc
  
  # Create a new data frame for probabilities
  probability <- NULL
  
  # Pnorm catches left side probabilities, so 0 to first bin
  # and pops that into our probability table
  probability[1] <- pnorm(bins[1], df_adj_mean, df_sd) 
  
  # Skipping first bin, calculate probability of left of bin 
  # minus the probability of everything before previous bin
  for (i in 2:length(bins)) {
    probability[i] <- pnorm(bins[i], df_adj_mean, df_sd) - pnorm(bins[i-1], df_adj_mean, df_sd) 
  }

  # Tell me what the assumed mean was, standard deviation and probabilities
  return(cat(paste0("Projected mean: ", round(df_adj_mean, digits = 0), "\n",
                "Standard deviation: ", round(df_sd, digits = 0), "\n\n",
                "Base rate probability: \n", 
                "Bin 1: ", round(probability[1], digits = 3), "\n",
                "Bin 2: ", round(probability[2], digits = 3), "\n",
                "Bin 3: ", round(probability[3], digits = 3), "\n",
                "Bin 4: ", round(probability[4], digits = 3), "\n",
                "Bin 5: ", round(probability[5], digits = 3), "\n",
                "\n")))
}