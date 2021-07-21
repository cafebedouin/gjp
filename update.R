# update.R 
#################################################
# Description: Calls functions for forecasts
# requiring update.

source("fred.R")
fred(code="CSUSHPINSA",
     begin_date="1987-01-01", # For analysis, not question
     closing_date="2021-10-01",
     freq="monthly",
     trading_days=7, 
     bins=c(250, 260, 270, 280, Inf),
     probability_type="monte",
     title=paste0(code, " S&P/Case-Shiller US National Home Price Index "),
     subtitle="",
     info_source="FRED",
     file_name="FRED",
     graph_width=1250,
     graph_height=450,
     df_summary="yes") 

source("fred.R")
fred(code="HOUST",
     begin_date="1987-01-01", # For analysis, not question
     closing_date="2021-10-01",
     freq="monthly",
     trading_days=7, 
     bins=c(1400, 1600, 1800, 2000, Inf),
     probability_type="simple",
     title=paste0(code, " New Housing Starts "),
     subtitle="",
     info_source="FRED",
     file_name="FRED",
     graph_width=1250,
     graph_height=450,
     df_summary="yes") 


source("fred.R")
fred(code="TRUCKD11",
     begin_date="2000-01-01", # For analysis, not question
     closing_date="2022-01-30",
     freq="monthly",
     trading_days=7, 
     bins=c(110, 120, 130, 140, Inf),
     probability_type="simple",
     title=paste0(code, " Truck Tonnage "),
     subtitle="",
     info_source="FRED",
     file_name="FRED",
     graph_width=1250,
     graph_height=450,
     df_summary="yes") 

source("fred.R")
fred(code="CBBTCUSD",
     begin_date="2017-01-01", # For analysis, not question
     closing_date="2021-12-09",
     freq="daily",
     trading_days=7,
     bins=c(34000, 56000, 58000, 60000, Inf),
     probability_type="monte",
     title=paste0(code, " Crypto "),
     subtitle="",
     info_source="FRED",
     file_name="FRED",
     graph_width=1250,
     graph_height=450,
     df_summary="yes") 

source("fred.R")
fred(code="CPIAUCSL",
     begin_date="1947-01-01", # For analysis, not question
     closing_date="2021-09-01",
     freq="monthly",
     trading_days=7, 
     bins=c(-0.13, -0.126, -0.122, -0.118, Inf),
     probability_type="simple_percent",
     title=paste0(code, " CPI "),
     subtitle="",
     info_source="FRED",
     file_name="FRED",
     graph_width=1250,
     graph_height=450,
     df_summary="yes") 


# CPI Annual Percent
source("fred.R")
fred(code="CPALTT01USM659N",
     begin_date="1980-01-01", # For analysis, not question
     closing_date="2022-06-30",
     freq="monthly",
     trading_days=7, 
     bins=c(2, 3, 4, 5, Inf),
     probability_type="monte",
     title=paste0(code, " CPI "),
     subtitle="",
     info_source="FRED",
     file_name="FRED",
     graph_width=1250,
     graph_height=450,
     df_summary="yes") 

# Brent Crude
source("fred.R")
fred(code="DCOILBRENTEU",
     begin_date="1971-01-01", # For analysis, not question
     closing_date="2021-12-30",
     freq="daily",
     trading_days=7, 
     bins=c(70, 76.39, 78.39, 80.3, Inf),
     probability_type="simple",
     title=paste0(code, " Brent Crude "),
     subtitle="",
     info_source="FRED",
     file_name="FRED",
     graph_width=1250,
     graph_height=450,
     df_summary="yes") 

# Update Unemployment
source("fred.R")
fred(code="SPDYNCBRTINUSA",
     begin_date="2007-01-01", # For analysis, not question
     closing_date="2023-04-16",
     freq="yearly",
     trading_days=7, 
     bins=c(10.8, 11, 11.2, 11.4, Inf),
     probability_type="simple",
     title=paste0(code, " Unemployment "),
     subtitle="",
     info_source="FRED",
     file_name="FRED",
     graph_width=1250,
     graph_height=450,
     df_summary="yes") 

# Update CPI
source("fred.R")
fred(code="CPALTT01USM657N",
     begin_date="2015-01-01", # For analysis, not question
     closing_date="2021-11-13",
     freq="monthly",
     trading_days=5, 
     bins=c(6, 7, 7.5, 8, Inf),
     probability_type="simple",
     title=paste0(code, " Chinese Yuan / USD "),
     subtitle="",
     info_source="FRED",
     file_name="FRED",
     graph_width=1250,
     graph_height=450,
     df_summary="yes") 

# Update Chinese Yuan
source("fred.R")
fred(code="DEXCHUS",
     begin_date="2015-01-01", # For analysis, not question
     closing_date="2021-11-13",
     freq="daily",
     trading_days=5,
     bins=c(6, 7, 7.5, 8, Inf),
     probability_type="simple",
     title=paste0(code, " Chinese Yuan / USD "),
     subtitle="",
     info_source="FRED",
     file_name="FRED",
     graph_width=1250,
     graph_height=450,
     df_summary="yes") 

source("fred.R")
fred(code="SP500",
     begin_date="2010-01-01", # For analysis, not question
     closing_date="2022-06-30", 
     freq="daily",
     trading_days=5, 
     bins=c(3500, 3800, 4100, 4400, 4700, 5000, Inf),
     probability_type="simple",
     title=paste0(code, " S&P 500 "),
     subtitle="",
     info_source="FRED",
     file_name="FRED",
     graph_width=1250,
     graph_height=450) 

source("fred.R")
fred(code="CBBTCUSD",
     begin_date="2016-03-11", # For analysis, not question
     closing_date="2021-06-30", 
     freq="daily",
     trading_days=7, 
     bins=c(25000, 50000, 75000, 10000, Inf),
     probability_type="simple",
     graph="no",
     title=paste0(code, ", Coinbases's Bitcoin Price "),
     subtitle="",
     info_source="FRED",
     file_name="FRED",
     graph_width=1250,
     graph_height)

# Update Inflation in Europe
source("fred.R")
fred(code="CLVMNACSCAB1GQEA19", # Real GDP for euro Area
     begin_date="2014-01-01", # For analysis, not question
     closing_date="2020-04-01",
     freq="daily",
     trading_days=5,
     bins=c(63, 65, 67, 69, Inf),
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
     freq="daily",
     annual_percent="yes",
     trading_days=5, 
     bins=c(1, 2, 3, 4, Inf),
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
     bins=c(1150, 1200, 1250, 1300, Inf),
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
nasdaq(ticker="CCL", 
       begin_date="2013-01-01", # For analysis, not question
       closing_date="2022-03-01", 
       trading_days=5, 
       bins=c(20, 30, 40, 50, Inf),
       probability_type="simple",
       # If you want a graph, indicate and add info
       graph="no",
       title="",
       subtitle="",
       info_source="NASDAQ",
       file_name="CCL",
       graph_width=1250,
       graph_height=450)


source("nasdaq.R")
nasdaq(ticker="DOGE", 
       begin_date="2019-02-08", # For analysis, not question
       closing_date="2021-08-01", 
       trading_days=7, 
       bins=c(0.25, 0.5, 0.75, 1, Inf),
       probability_type="monte",
       # If you want a graph, indicate and add info
       graph="yes",
       title="",
       subtitle="",
       info_source="NASDAQ",
       file_name="DOGE",
       graph_width=1250,
       graph_height=450)


source("nasdaq.R")
nasdaq(ticker="U", 
       begin_date="2021-01-01", # For analysis, not question
       closing_date="2021-12-31", 
       trading_days=5, 
       bins=c(250, 300, 350, 400, Inf),
       probability_type="simple",
# If you want a graph, indicate and add info
       graph="yes",
       title="",
       subtitle="",
       info_source="NASDAQ",
       file_name="U",
       graph_width=1250,
       graph_height=450)

# Treasury Yield for 10 Years
source("yield.R")
yield(closing_date="2021-09-16",
      begin_year=2010, # > than this year, for analysis
      trading_days=7,
      freq="daily",
      bins=c(1.0, 1.5, 2.0, 2.5, Inf),
      probability_type="simple",
      prob_results_title="Treasury Yields for 10 Year",
      # If you want a graph, indicate and add info
      graph="no",
      title="Treasury Yields for 10 Year",
      subtitle="",
      info_source="U.S. Treasury",
      file_name="treasury",
      graph_width=1250,
      graph_height=450,
      # Script does analysis for only one Treasury Yield at a time.                    
      # Yield Codes:  Y1M = 1 Month Yield, Y2M = 2 Month Yield Y3M, Y6M, 
      # Y1Y = 1 Year Yield, Y2Y, Y3Y, Y5Y, Y7Y, Y10Y, Y20Y, Y30Y                       
      treasury_code="Y10Y")

# Treasury Yield for 30 Years
source("yield.R")
yield(closing_date="2021-12-31",
      begin_year=2010, # > than this year, for analysis
      trading_days=7, 
      freq="daily",
      bins=c(1.5, 2.0, 2.5, 3.0, Inf),
      probability_type="monte",
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

# Update Unemployment
source("fred.R")
fred(code="GDPC1",
     begin_date="1947-01-01", # For analysis, not question
     closing_date="2022-03-01",
     freq="quarterly",
     trading_days=7, 
     bins=c(1.5, 2.0, 2.5, 3.0, Inf),
     probability_type="simple",
     title=paste0(code, " Unemployment "),
     subtitle="",
     info_source="FRED",
     file_name="FRED",
     graph_width=1250,
     graph_height=450,
     df_summary="yes") 