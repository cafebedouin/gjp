# defun_update.R 
#################################################
# Description: Calls functions for forecasts
# requiring update.

# Update Chinese Yuan
source("fred.R")
fred(code="DEXCHUS",
     begin_date="2015-01-01", # For analysis, not question
     closing_date="2020-04-17", 
     freq="daily",
     trading_days=5, 
     bin1=6, 
     bin2=7, 
     bin3=7.5, 
     bin4=8,
     probability_type="simple",
     graph="no",
     title=paste0(code, " Chinese Yuan / USD "),
     subtitle="",
     info_source="FRED",
     file_name="FRED",
     graph_width=1250,
     graph_height=450) 

# Update Brent Oil
source("fred.R")
fred(code="DCOILBRENTEU",
     begin_date="2014-01-01", # For analysis, not question
     closing_date="2020-04-01", 
     trading_days=5, 
     bin1=63, 
     bin2=65, 
     bin3=67, 
     bin4=69,
     probability_type="simple",
# If you want a graph, indicate and add info
     graph="no",
     title=paste0(code, " Brent Oil Prices "),
                  subtitle="",
                  info_source="",
                  file_name="FRED",
                  graph_width=1250,
                  graph_height=450) 

# Update Inflation in Europe
source("fred.R")
fred(code="CLVMNACSCAB1GQEA19",
     begin_date="2014-01-01", # For analysis, not question
     closing_date="2020-04-01", 
     trading_days=5, 
     bin1=63, 
     bin2=65, 
     bin3=67, 
     bin4=69,
     probability_type="simple",
# If you want a graph, indicate and add info
     graph="no",
     title=paste0(code, " Brent Oil Prices "),
     subtitle="",
     info_source="",
     file_name="FRED",
     graph_width=1250,
     graph_height=450) 

source("nasdaq.R")
nasdaq(ticker="TSLA", 
       begin_date="2014-01-01", # For analysis, not question
       closing_date="2020-12-31", 
       trading_days=5, 
       bin1=250, 
       bin2=300, 
       bin3=350, 
       bin4=400,
       probability_type="simple",
# If you want a graph, indicate and add info
       print_graph="yes",
       title="",
       subtitle="",
       info_source="NASDAQ",
       file_name="TSLA",
       graph_width=1250,
       graph_height=450)

# Treasury Yield for 30 Years
source("yield.R")
yield(closing_date="2020-12-31",
      begin_year=2013, # > than this year, for analysis
      trading_days=7, 
      bin1=0.0, 
      bin2=2.0, 
      bin3=2.5, 
      bin4=3.0,
      probability_type="simple",
      prob_results_title="Treasury Yields for 30 Year",
# If you want a graph, indicate and add info
      graph="yes",
      title="Treasury Yields for 30 Year",
      subtitle="",
      info_source="U.S. Treasury",
      file_name="treasury",
      graph_width=1250,
      graph_height=450,
# Script does analysis for only one Treasury Yield at a time.                    
# Yield Codes:  Y1M = 1 Month Yield, Y2M = 2 Month Yield Y3M, Y6M, 
# Y1Y = 1 Year Yield, Y2Y, Y3Y, Y5Y, Y7Y, Y10Y, Y20Y, Y30Y                       
      treasury_code="Y30Y")