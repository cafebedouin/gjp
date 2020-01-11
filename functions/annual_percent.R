# annual_percent.R 
#################################################
# Description: Changes a column of values into 
# a percentage.


annual_percent <- function(df, 
                           freq="monthly") {

  #################################################
  # Load libraries. If library X is not installed
  # you can install it with this command at the R prompt:
  # install.packages('X')
  library(dplyr)
  library(tidyr)
  
  # Translate freq to a number
  if (freq == "daily") { freq <- 365 } 
  if (freq == "weekly") { freq <- 52 }
  if (freq == "monthly") { freq <- 12 }  
  if (freq == "quarterly") { freq <- 4 }
  if (freq == "yearly") {  freq <- 1 }

  # Make a new column, dividing by freq rows down,
  # which assumes reverse chronological order
  for (i in 1:((length(df$value)) - freq)) {
    df[i, 3] <- abs(round(sum(sum(1-(sum(df[i, 2] / df[(i + freq), 2])))*100), digits = 1))
  }

  # Drop the previous value column
  df <- df[, -c(2)]
  
  # Get value for extra NA rows
  krows <- length(df$value)-freq
  
  # Drop the extra rows
  df <- drop_na(df)
  
  # Fix the names
  colnames(df) <- c("date", "value")
  
  # Testing
  View(df)
  
  # Send it back
  return(df)
}