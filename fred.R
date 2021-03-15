# fred.R 
#################################################
# Description: Pulls data from the FRED website on 
# currencies, precious metals, unemployment, etc.
# allows you to limit the data set, set the 
# frequency interval, then provides 
# a probability base line against <=5 bins or 
# 4 price points using current price and walk 
# through the outcomes in set.
# 
# Use: Best called as a function from another script
# where defaults for multiple questions can be stored 
# and run at will, e.g., update.R, in this repo.
#
# Example FRED data 
#################################################
# Exchange Rate codes
# U.S. / Australia Foreign Exchange Rate (DEXUSAL)
# Brazil / U.S. Foreign Exchange Rate (DEXBZUS)
# Canada / U.S. Foreign Exchange Rate (DEXCAUS)
# China / U.S. Foreign Exchange Rate (DEXCHUS)
# Denmark / U.S. Foreign Exchange Rate (DEXDNUS)
# U.S. / Euro Foreign Exchange Rate (DEXUSEU)
# Hong Kong / U.S. Foreign Exchange Rate (DEXHKUS)
# India / U.S. Foreign Exchange Rate (DEXINUS)
# Japan / U.S. Foreign Exchange Rate (DEXJPUS)
# U.S. / New Zealand Foreign Exchange Rate (DEXUSNZ)
# Malaysia / U.S. Foreign Exchange Rate (DEXMAUS)
# Mexico / U.S. Foreign Exchange Rate (DEXMXUS) 
# Norway / U.S. Foreign Exchange Rate (DEXNOUS)
# Singapore / U.S. Foreign Exchange Rate (DEXSIUS)
# South Africa / U.S. Foreign Exchange Rate (DEXSFUS)
# South Korea / U.S. Foreign Exchange Rate (DEXKOUS)
# Sri Lanka / U.S. Foreign Exchange Rate (DEXSLUS)
# Sweden / U.S. Foreign Exchange Rate (DEXSDUS)
# Switzerland / U.S. Foreign Exchange Rate (DEXSZUS)
# Taiwan / U.S. Foreign Exchange Rate (DEXTAUS)
# Thailand / U.S. Foreign Exchange Rate (DEXTHUS)
# U.S. / U.K. Foreign Exchange Rate (DEXUSUK)
# Venezuela / U.S. Foreign Exchange Rate (DEXVZUS)
#
# Precious Metal codes
# Gold Price, 10:30 A.M., LBMA, U.S. Dollars (GOLDAMGBD228NLBM)
# Gold Price, 3:00 P.M., LBMA, U.S. Dollars (GOLDPMGBD228NLBM)
# Gold Price, 10:30 A.M., British Pounds (GOLDAMGBD229NLBM)
# Gold Price, 3:00 P.M., LBMA, British Pounds (GOLDPMGBD229NLBM)
# Gold Price, 10:30 A.M., LBMA, Euros (GOLDAMGBD230NLBM)
# Gold Price, 3:00 P.M., LBMA, Euros (GOLDPMGBD230NLBM)
# Silver Price, 12:00 noon, LBMA, U.S. Dollars (SLVPRUSD)
# Silver is available in many currency denominations.
#
# Indices
# NASDAQ, S&P, Dow
# NASDAQ Composite Index (NASDAQCOM)
# NASDAQ 100 Index (NASDAQ100)
# S&P 500 (SP500)
# Dow Jones Industrial Average (DJIA)
# Dow Jones Transportation Average (DJTA)
# Dow Jones Utility Average (DJUA)
# Dow Jones Composite Average (DJCA)
#
# Misc
# Crude Oil Prices: Brent - Europe (DCOILBRENTEU)
# University of Michigan: Consumer Sentiment (UMCSENT)


# Replace defaults in function to desired, or 
# call the function from another script or console
fred <- function(code, # FRED, e.g., SP500 is S&P 500 in FRED
                 begin_date, # For analysis, not question
                 closing_date, # for question
                 bin1, 
                 bin2, 
                 bin3, 
                 bin4,
                 # Type of analysis, simple historical, monte carlo, etc.
                 probability_type="simple",
                 freq="daily", # daily, weekly, monthly, quarterly, yearly
                 trading_days=5, # Only matters for daily freq
                 annual_percent="no", # use function to convert 
                 prob_results_title=paste0(code,
                                           " probability table run on ",
                                           todays_date),
                 # If you want a graph, indicate and add info
                 graph="no",
                 title=paste0(code, " Title "),
                 subtitle="",
                 info_source="",
                 file_name="",
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
  
  # Set date for ten years ago, FRED often has a 10 year limit
  ten_years <- todays_date - 3652
  
  #################################################
  # Load libraries. If library X is not installed
  # you can install it with this command at the R prompt:
  # install.packages('X')
  library(data.table)
  library(dplyr)
  
  #################################################
  # Import, organize and output csv data
  
  # Create live url to access data
  nurl = paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?",
                "&id=",
                code, "&cosd=",
                begin_date, "&coed=",
                todays_date, 
                "mark_type=none&mw=3&lw=2&ost=-99999&",
                "oet=99999&mma=0&fml=a&fq=Daily&fam=avg",
                "&fgst=lin&fgsnd=",
                ten_years, "&line_index=1",
                "&transformation=lin&vintage_date=",
                todays_date, "&revision_date=",
                todays_date, "&nd=",
                begin_date)
  
  # Live import
  df <- read.csv(nurl, skip=0, header=TRUE)
  
  # Troubleshoot
  ###############################################
  # Writes csv file, so you can use next bit of code
  # without constantly pulling from FRED when testing.
  #
  # write.csv(df, file=paste0("./data/FRED-", 
  #                          code,
  #                          "-",
  #                          todays_date,
  #                          ".csv")) 
  #
  # Downloaded data rather than live, name same as FRED code
  # df <- read.csv(paste0("./data/FRED-", 
  #                      code, 
  #                      "-",
  #                      todays_date,                       
  #                      ".csv"), 
  #                      skip=0, header=TRUE)
  # 
  # In case columns need to be changed.
  # df <- df[ -c(1) ]
  
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
  write.csv(df, file=paste0("./output/FRED-", 
                            code,
                            "-",
                            todays_date,
                            ".csv")) 

  #################################################
  # Call desired forecasting functions
  
  # Simple walk through historical values to generate probabilities
  if (probability_type == "simple") {
    source("./functions/simple_probability.R")
    simple_probability(df, prob_results_title,
                       closing_date, trading_days, freq,
                       bin1, bin2, bin3, bin4) }
  
  # Makes graphs
  if (graph == "yes") {
    source("./functions/graph.R")  
    graph(df, title, subtitle, info_source, file_name, 
          graph_width, graph_height)
  }
} 
