# defun_nasdaq.R 
#################################################
# Description: Pulls NASDAQ data by ticker and 
# from the NASDAQ website, allows you to limit the 
# data set, then provides a probability base line
# against <=5 bins or 4 price points using current
# price and walk through the outcomes in set.

# Replace defaults in function to desired, or 
# call the function from console
defun_nasdaq <- function(ticker="TSLA", 
                       begin_date="2014-01-01", # For analysis, not question
                       closing_date="2020-12-31", 
                       trading_days=5, 
                       bin1=250, 
                       bin2=300, 
                       bin3=350, 
                       bin4=400,
                       probability_type="simple",
                       prob_results_title=paste0(ticker, 
                                                 " Probability Table"),
                       # If you want a graph, indicate and add info
                       print_graph="yes",
                       title=paste0(ticker," Historical Prices"),
                       subtitle="",
                       info_source="NASDAQ",
                       file_name="TSLA",
                       graph_width=1250,
                       graph_height=450) {
  
  #################################################
  # Preliminaries
  
  # Define basepath and set working directory:
  basepath = "~/Documents/R/forecasting"
  setwd(basepath)
  
  # Preventing scientific notation in graphs
  options(scipen=999)
  
  # Set todays_date
  todays_date <- Sys.Date()
  
  #################################################
  # Load libraries. If library X is not installed
  # you can install it with this command at the R prompt:
  # install.packages('X')
  library(data.table)
  
  # Sources frequently called forecasting functions
  source("./functions/defun_graph.R")
  source("./functions/defun_simple_probability.R")

  #################################################
  # Import, organize and output csv data
  
  # Create url to access data
  nurl = paste0("https://www.nasdaq.com/api/v1/historical/",
                ticker, "/stocks/", begin_date, "/", todays_date)
  
  # Live import
  df <- read.csv(nurl, skip=0, header=TRUE)
  View(df)
  
  # Downloaded
  # df <- read.csv("/home/scott/Downloads/HistoricalQuotes.csv", 
  #                   skip=0, header=TRUE)

  # Selects date and closing value
  df = df[, -c(3:6)]
  
  # Names the columns
  colnames(df) <- c("date", "value")
  
  # Translates the NASDAQ date into one R likes
  df$date <- as.Date(df$date, "%m/%d/%Y")
  
  # Removes the dollar signs from values
  df$value <- as.numeric(gsub("[\\$,]", "", df$value))
  
  # Making sure data types are in the correct format
  df$date <- as.Date(df$date)
  df$value <- as.vector(df$value)

  #################################################
  # Call desired forecasting functions
  
  if (probability_type == "simple")
    defun_simple_probability(df, prob_results_title,
                             closing_date, trading_days, 
                             bin1, bin2, bin3, bin4)
  
  if (print_graph == "yes")
    defun_graph(df, title, subtitle, info_source, file_name, 
                graph_width, graph_height)
}