# year_day_matrix_cvs.R 
#################################################
# Description: Takes data frame of dates, values
# and turns it into year, day, value and then
# puts it into a matrix, exported to csv.

matrix_probability <- function(df,
                               file_name) {
  todays_date <- Sys.Date()
  
  # Naming columns
  colnames(df) <- c("year", "day", "value")

  df$year <- as.Date(df$year)
  df$day <- as.Date(df$year) 

  # Puts into date decreasing order
  # df <- df[rev(order(as.Date(df$Year), na.last = NA)),]

  df$day <- format(df$day, "%j")
  df$year <- format(df$year, "%Y")

  # Reshape the three columns into a table, fix names, and export csv file
  df <- reshape(df, direction="wide", idvar="day", timevar="year")
  names(df) <- gsub("value.", "", names(df))

  # Creating a cvs file of changed data
  write.csv(df, file=paste0("./output/", filename, "-", todays_date, ".csv")) 
}