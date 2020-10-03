# defun_update.R 
#################################################
# Description: Calls functions for forecasts
# requiring update.

# Update Chinese Yuan
source("fred.R")
fred(code="DEXCHUS",
     begin_date="2015-01-01", # For analysis, not question
     closing_date="2020-11-13", 
     freq="daily",
     trading_days=5, 
     bin1=6, 
     bin2=7, 
     bin3=7.5, 
     bin4=8,
     probability_type="simple",
     title=paste0(code, " Chinese Yuan / USD "),
     subtitle="",
     info_source="FRED",
     file_name="FRED",
     graph_width=1250,
     graph_height=450) 

source("fred.R")
fred(code="SP500",
     begin_date="2015-01-01", # For analysis, not question
     closing_date="2020-07-01", 
     freq="daily",
     trading_days=5, 
     bin1=2047, 
     bin2=2088, 
     bin3=2529, 
     bin4=2770,
     probability_type="simple",
     title=paste0(code, " S&P 500 "),
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
     freq = "daily",
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
fred(code="CLVMNACSCAB1GQEA19", # Real GDP for euro Area
     begin_date="2014-01-01", # For analysis, not question
     closing_date="2020-04-01", 
     trading_days=5, 
     bin1=63, 
     bin2=65, 
     bin3=67, 
     bin4=69,
     freq="monthly",
     probability_type="simple",
# If you want a graph, indicate and add info
     graph="no",
     title=paste0(code, " Brent Oil Prices "),
     subtitle="",
     info_source="",
     file_name="FRED",
     graph_width=1250,
     graph_height=450) 

# Update Inflation in United States
source("fred.R")
fred(code="GDPC1",
     begin_date="1947-01-01", # For analysis, not question
     closing_date="2020-12-01",
     freq="yearly", 
     annual_percent="yes",
     trading_days=5, 
     bin1=1, 
     bin2=2, 
     bin3=3, 
     bin4=4,
     probability_type="simple",
     # If you want a graph, indicate and add info
     graph="no",
     title=paste0(code, " Negative Real GDP "),
     subtitle="",
     info_source="",
     file_name="FRED",
     graph_width=1250,
     graph_height=450) 


# Update Won
source("fred.R")
fred(code="DEXKOUS",
     begin_date="2000-01-01", # For analysis, not question
     closing_date="2020-07-15",
     freq="daily", 
     trading_days=5, 
     bin1=1150, 
     bin2=1200, 
     bin3=1250, 
     bin4=1300,
     probability_type="simple",
     # If you want a graph, indicate and add info
     graph="no",
     title=paste0(code, " Korean Won "),
     subtitle="",
     info_source="",
     file_name="FRED",
     graph_width=1250,
     graph_height=450) 

source("nasdaq.R")
nasdaq(ticker="UBER:US", 
       begin_date="2015-01-01", # For analysis, not question
       closing_date="2020-12-31", 
       trading_days=5, 
       bin1=20, 
       bin2=29, 
       bin3=38, 
       bin4=46,
       probability_type="simple",
       # If you want a graph, indicate and add info
       graph="no",
       title="",
       subtitle="",
       info_source="NASDAQ",
       file_name="UBER",
       graph_width=1250,
       graph_height=450)

source("nasdaq.R")
nasdaq(ticker="TSLA", 
       begin_date="2015-01-01", # For analysis, not question
       closing_date="2020-12-31", 
       trading_days=5, 
       bin1=250, 
       bin2=300, 
       bin3=350, 
       bin4=400,
       probability_type="simple",
# If you want a graph, indicate and add info
       graph="yes",
       title="",
       subtitle="",
       info_source="NASDAQ",
       file_name="TSLA",
       graph_width=1250,
       graph_height=450)

# Treasury Yield for 30 Years
source("yield.R")
yield(closing_date="2020-09-22",
      begin_year=2015, # > than this year, for analysis
      trading_days=7, 
      bin1=1.2, 
      bin2=1.4, 
      bin3=1.6, 
      bin4=1.8,
      probability_type="simple",
      prob_results_title="Treasury Yields for 30 Year",
      # If you want a graph, indicate and add info
      graph="no",
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



# Treasury Yield for 30 Years
source("yield.R")
yield(closing_date="2020-12-31",
      begin_year=2015, # > than this year, for analysis
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