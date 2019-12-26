# defun_yield.R 
#################################################
# Description: This script is for scraping yield data
# from the Treasury website limiting to the yield of
# interest then doing a simple walk through historical
# data to project probabilities for future Treasury 
# yields on some arbitrary date.

#Clear memory and set string option for reading in data:
rm(list=ls())
gc()

#################################################
# Function
defun_yield <- function(closing_date="2020-12-31",
                        start_date=2013, # < than this year, for analysis
                        trading_days=7, 
                        bin1=0.0, 
                        bin2=2.0, 
                        bin3=2.5, 
                        bin4=3.0,
                        code="Y30Y") {
    
    # Yield Codes:  Y1M = 1 Month Yield, Y2M = 2 Month Yield Y3M, Y6M, 
    # Y1Y = 1 Year Yield, Y2Y, Y3Y, Y5Y, Y7Y, Y10Y, Y20Y, Y30Y 

  #################################################
  # Libraries
  #
  # Load libraries. If library X is not installed
  # you can install it with this command at the R prompt:
  # install.packages('X') 
  library(xml2)
  library(lubridate)
  library(dplyr)
    
  # Last 4 digits are year, retrieves years > than start_date
  yurl = paste0("http://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData?",
                "$filter=year(NEW_DATE)%20gt%20",
                start_date)
  
  #################################################
  # Import & Parse
  # Point to time series data file and import it.

  # Script testing with downloaded file
  # xmlobj <- read_xml("~/Downloads/DailyTreasuryYieldCurveRateData.xml")

  # Live script
  xmlobj <- read_xml(yurl)
  
  df_yield <- data.frame( 
    id   = xml_find_all(xmlobj, ".//d:Id" ) %>% xml_attr( "id" ),
    date = xml_find_all(xmlobj, ".//d:NEW_DATE" ) %>% xml_text(),
    Y1M = xml_find_all(xmlobj, ".//d:BC_1MONTH" ) %>% xml_text(),
    Y2M = xml_find_all(xmlobj, ".//d:BC_2MONTH" ) %>% xml_text(),
    Y3M = xml_find_all(xmlobj, ".//d:BC_3MONTH" ) %>% xml_text(),
    Y6M = xml_find_all(xmlobj, ".//d:BC_6MONTH" ) %>% xml_text(),
    Y1Y = xml_find_all(xmlobj, ".//d:BC_1YEAR" ) %>% xml_text(),
    Y2Y = xml_find_all(xmlobj, ".//d:BC_2YEAR" ) %>% xml_text(),
    Y3Y = xml_find_all(xmlobj, ".//d:BC_3YEAR" ) %>% xml_text(),
    Y5Y = xml_find_all(xmlobj, ".//d:BC_5YEAR" ) %>% xml_text(),
    Y7Y = xml_find_all(xmlobj, ".//d:BC_7YEAR" ) %>% xml_text(),
    Y10Y = xml_find_all(xmlobj, ".//d:BC_10YEAR" ) %>% xml_text(),
    Y20Y = xml_find_all(xmlobj, ".//d:BC_20YEAR" ) %>% xml_text(),
    Y30Y = xml_find_all(xmlobj, ".//d:BC_30YEAR" ) %>% xml_text(),
    Y30D = xml_find_all(xmlobj, ".//d:BC_30YEARDISPLAY" ) %>% xml_text(),
    stringsAsFactors = FALSE )

  # All of this is to determine how many days until end of question
  todays_date <- Sys.Date()
  closing_date <- as.Date(closing_date)
  remaining_weeks <- as.numeric(difftime(closing_date, todays_date, units = "weeks"))
  remaining_weeks <- round(remaining_weeks, digits=0)
  non_trading_days <- (7 - trading_days) * remaining_weeks
  day_difference <- as.numeric(difftime(closing_date, todays_date))
  remaining_days <- day_difference - non_trading_days 

  #################################################
  # Import & Parse
  # Point to time series data file and import it.
  time_import <- df_yield[,c("date", code)]
  
  # Changing column name to reuse time-series script
  colnames(time_import) <- c("date", "value")
  
  # time_import$date <- as.numeric(gsub("[T,]", "", nasdaq$value))
  #  as.Date(time_import$date, "%Y-%m-%dT%h:%m:%s")

  # Clips off time and imports value as number
  time_import$date <- as.Date(time_import$date)
  time_import$value <- as.numeric(time_import$value) 
  
  # Puts into date decreasing order
  time_import <- time_import[rev(order(as.Date(time_import$date), na.last = NA)),]
  
  # Putting into vectors format
  time_import$value <- as.vector(time_import$value) 
  time_import$date <- as.Date(time_import$date)
    
  # Filter out old data
  time_import <- filter(time_import, date > start_date)

  # Setting most recent value, assuming descending data
  current_value <- as.numeric(time_import[1,2])

  # Get the length of time_import$value and shorten it by remaining_days
  time_rows = length(time_import$value) - remaining_days

  # Create a dataframe
  time_calc <- NULL

  # Iterate through value and subtract the difference 
  # from the row remaining days away.
  for (i in 1:time_rows) {
    time_calc[i] <- time_import$value[i] - time_import$value[i+remaining_days]
  }

  # Adjusted against current values to match time_calc
  adj_bin1 <- bin1 - current_value
  adj_bin2 <- bin2 - current_value
  adj_bin3 <- bin3 - current_value 
  adj_bin4 <- bin4 - current_value 

  # Empirically, how many trading days fall in each question bin?
  prob1 <- round(sum(time_calc<adj_bin1)/length(time_calc), digits = 3)
  prob2 <- round(sum(time_calc>=adj_bin1 & time_calc<=adj_bin2)/length(time_calc), digits = 3)
  prob3 <- round(sum(time_calc>adj_bin2 & time_calc<adj_bin3)/length(time_calc), digits = 3)
  prob4 <- round(sum(time_calc>=adj_bin3 & time_calc<=adj_bin4)/length(time_calc), digits = 3)
  prob5 <- round(sum(time_calc>adj_bin4)/length(time_calc), digits = 3)
  
  ###############################################
  # Print results
  return(cat(paste0(code, " Treasury Yield Probabilities for ", closing_date, ":\n", 
                  prob1, ": Bin 1 - ", "<", bin1, "\n",
                  prob2, ": Bin 2 - ", bin1, " to <=", bin2, "\n", 
                  prob3, ": Bin 3 - ", bin2, "+ to <", bin3, "\n", 
                  prob4, ": Bin 4 - ", bin3, " to <=", bin4, "\n", 
                  prob5, ": Bin 5 - ", bin4, "+", "\n",
                  "Number of observations: ", time_rows)))
}