# brent.R 
#################################################
# Description: Pulls Brent Crude Oil pricing data 
# from the EIA website, allows you to limit the 
# data set, then provides a probability base line
# against <=5 bins or 4 price points using current
# price and walk through the outcomes in set.
# Set the parameters in the function, and then
# call the function as part of an update.

#Clear memory and set string option for reading in data:
rm(list=ls())
gc()

#################################################
# Function
brent <- function(closing_date="2020-04-01",
                  start_date="2014-01-01",
                  trading_days=5, 
                  freq="daily",
                  bin1=63, 
                  bin2=69, 
                  bin3=200, 
                  bin4=300,
# Called Functions
#################################################                                    
# Graph function:
                  graph = "no",
#                  prob_results_title="Brent Crude Oil Probablity Table ",
#                  title="Brent Crude Oil Historical Prices",
#                  subtitle="",
#                  info_source="Source: U.S. Energy Information Administration",
#                  file_name="brent",
#                  graph_width=1250,
#                  graph_height=450

# Simple probability function                  
                  probability_type = "simple",
                  prob_results_title="Brent Crude Oil Probablity Table ") {
  #################################################
  # Libraries
  #
  # Load libraries. If library X is not installed
  # you can install it with this command at the R prompt:
  # install.packages('X')
  library(readxl)
  library(xml2)
  library(lubridate)
  library(dplyr)
  library(httr)
  
  #################################################
  # Import & Parse
  # Point to time series data file and import it.
  
  # Necessary kludge because read_excel doesn't know about urls yet
   kludge <- tempfile(fileext = ".xls")
   GET(url = "http://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls",
       write_disk(kludge), progress())
  
  # This should work whenever read_excel learns about the web
  # df <- read_excel("http://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls", 
  #                           sheet = "Data 1")
  #
  # Uncomment for testing against downloaded file.
  # df <- read_excel("~/Downloads/RBRTEd.xls", sheet = "Data 1")
  
  df <- read_excel(kludge, sheet = "Data 1")
  df <- df[-c(1,2),]
  colnames(df) <- c("date", "value")

  # Cleaning up import
  df$date <- as.numeric(df$date)
  df$date <- as.Date(df$date, origin = "1899-12-30")
  df$value <- as.vector(as.numeric(df$value))
  df <- df[rev(order(df$date)),]   

  # Filter out old data
  df <- filter(df, date >= start_date)
  
  #################################################
  # Function Calls
  if (probability_type == "simple")
    source("./functions/simple_probability.R")
    simple_probability(df, prob_results_title,
                       closing_date, freq, trading_days, 
                       bin1, bin2, bin3, bin4)
    
  if (graph == "yes")
    source("./functions/graph.R")
    graph(df, title, subtitle, info_source, file_name, 
          graph_width, graph_height)
}  
