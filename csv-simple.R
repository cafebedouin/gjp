# cvs-simple.R 
#################################################
# Description: Pulls data from the FRED website on 
# currencies, precious metals, unemployment, etc.
# allows you to limit the data set, set the 
# frequency interval, then provides 
# a probability base line against <=5 bins or 
# 4 price points using current price and walk 
# through the outcomes in set.


# Replace defaults in function to desired, or 
# call the function from console
csv <- function(begin_date="2018-05-13", # For analysis, not question
                closing_date="2021-07-01",
                trading_days=5, 
                bin1=0.50, 
                bin2=1, 
                bin3=2.5, 
                bin4=5,
                probability_type="simple",
                annual_percent="no",
                prob_results_title="ADA cryto probability table",
                 # If you want a graph, indicate and add info
                graph="no",
                title=paste0(code, " Title "),
                subtitle="",
                info_source="ADP",
                file_name="ADP",
                graph_width=1250,
                graph_height=450,
                df_summary="yes") {
  
  #################################################
  # Preliminaries
  
  # Define basepath and set working directory:
  basepath = "~/Documents/R/forecasting"
  setwd(basepath)
  
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
  df <- read.csv(paste0("~/Downloads/ADA_USD_2018-05-31_2021-03-12-CoinDesk.csv"), 
                        skip=0, header=TRUE)

  # Remove unwanted columns, if it hasn't been done already.
  df <- df[ -c(1,4,5,6) ]
  
  # Names the columns
  colnames(df) <- c("date", "value")
  
  # Filter out days with value of "."
  df <- filter(df, value != ".") 
  df %>% is.na()
  
  # Making sure data types are in the correct format
  df$date <- as.Date(df$date)
  df$value <- as.character(df$value) # for converting factors
  df$value <- as.numeric(df$value)
  
  # Reverse dates 
  df <- df[rev(order(df$date)),] 

  # Visually check columns, data types and data
  glimpse(df)
  
  # Writes csv file
  write.csv(df, file=paste0("./output/csv-", 
                            todays_date,
                            ".csv")) 

  #################################################
  # Call desired forecasting functions
  
  # Simple walk through historical values to generate probabilities
  if (probability_type == "simple") {
    source("./functions/simple_probability.R")
    simple_probability(df, prob_results_title,
                       closing_date, trading_days,
                       bin1, bin2, bin3, bin4) }
  
  # Makes graphs
  if (graph == "yes") {
    source("./functions/graph.R")  
    graph(df, title, subtitle, info_source, file_name, 
          graph_width, graph_height)
  }
} 
