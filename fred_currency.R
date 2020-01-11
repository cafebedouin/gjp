# fred_currency.R 
#################################################
# Description: Pulls data from the FRED website on 
# currencies and allows you to limit the 
# data set, then provides a probability base line
# against <=5 bins or 4 price points using current
# price and walk through the outcomes in set.
# 
# Exchange Rate codes:
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

# Replace defaults in function to desired, or 
# call the function from console
fred_currency <- function(code="DEXHKUS",
                       begin_date="2009-01-01", # For analysis, not question
                       closing_date="2020-06-12", 
                       trading_days=5, 
                       bin1=7.851, 
                       bin2=10, 
                       bin3=20, 
                       bin4=30,
                       probability_type="simple",
                       prob_results_title=paste0(currency, 
                                                " Currency Probability Table"),
                       # If you want a graph, indicate and add info
                       print_graph="no",
                       title=paste0(currency, " Historical Prices"),
                       subtitle="",
                       info_source="FRED",
                       file_name="FRED",
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
  
  # Set date for ten years ago
  ten_years <- todays_date - 3652
  
  #################################################
  # Load libraries. If library X is not installed
  # you can install it with this command at the R prompt:
  # install.packages('X')
  library(data.table)
  library(dplyr)
  
  # Sources frequently called forecasting functions
  source("./functions/defun_graph.R")
  source("./functions/defun_simple_probability.R")
  
  #################################################
  # Import, organize and output csv data
  
  # Create url to access data
  nurl = paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?",
                "bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans",
                "&graph_bgcolor=%23ffffff&height=450&mode=fred&",
                "recession_bars=on&txtcolor=%23444444&ts=12&tts=12",
                "&width=1168&nt=0&thu=0&trc=0&show_legend=yes&",
                "show_axis_titles=yes&show_tooltip=yes&id=",
                currency, "&scale=left&cosd=",
                begin_date, "&coed=",
                todays_date, "&line_color=%234572a7&",
                "link_values=false&line_style=solid&",
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
  
  # Downloaded
  # df <- read.csv("~/Downloads/DEXUSEU.csv", 
  #                   skip=0, header=TRUE)

  # Names the columns
  colnames(df) <- c("date", "value")
  
  # Filter out days with value of "."
  df <- filter(df, value != ".") 
  
  # Reverse dates 
  df <- df[rev(order(df$date)),]   
  
  # Making sure data types are in the correct format
  df$date <- as.Date(df$date)
  df$value <- as.vector(df$value)
  
  #################################################
  # Call desired forecasting functions
  if (probability_type == "simple")
    source("./functions/simple_probability.R")
    simple_probability(df, prob_results_title,
                       closing_date, trading_days, 
                       bin1, bin2, bin3, bin4)

  if (graph == "yes")
    source("./functions/graph.R")   
    graph(df, title, subtitle, info_source, file_name, 
          graph_width, graph_height)
}