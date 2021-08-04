# probform.R 
#################################################
# Description: Calculates formats for probability functions

probform <- function(df) {
  # Changing column name to required values
  colnames(df) <- c("date", "value")
  
  # Clips off time and imports value as number
  df$date <- as.Date(df$date)
  df$value <- as.numeric(df$value) 
  
  # Puts into date decreasing order
  df <- df[rev(order(as.Date(df$date), na.last = NA)),]
  
  # Removes NAs if they appear in values column
  # df[!is.na(df[, 2]), ]
  
  # View(df) # for testing
  
  return(df)
}