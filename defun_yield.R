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
                        begin_year=2013, # > than this year, for analysis
                        trading_days=7, 
                        bin1=0.0, 
                        bin2=2.0, 
                        bin3=2.5, 
                        bin4=3.0,
                        probability_type="simple",
                        prob_results_title="Treasury Yields for 30 Year",
    # If you want a graph, indicate and add info
                        print_graph="yes",
                        title="Treasury Yields for 30 Year",
                        subtitle="",
                        info_source="U.S. Treasury",
                        file_name="treasury",
                        graph_width=1250,
                        graph_height=450,
    # Script does analysis for only one Treasury Yield at a time.                    
    # Yield Codes:  Y1M = 1 Month Yield, Y2M = 2 Month Yield Y3M, Y6M, 
    # Y1Y = 1 Year Yield, Y2Y, Y3Y, Y5Y, Y7Y, Y10Y, Y20Y, Y30Y                       
                        treasury_code="Y30Y") {

  #################################################
  # Libraries
  #
  # Load libraries. If library X is not installed
  # you can install it with this command at the R prompt:
  # install.packages('X') 
  library(xml2)
  library(lubridate)
  library(dplyr)
  
  # Sources frequently called forecasting functions
  source("./functions/defun_graph.R")
  source("./functions/defun_simple_probability.R")
    
  #################################################
  # Import & Parse Treasury Data
  # Point to time series data file and import it.
  # Last 4 digits are year, retrieves years > than start_date
  yurl = paste0("http://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData?",
                "$filter=year(NEW_DATE)%20gt%20",
                begin_year)  

  # Script testing with downloaded file
  # xmlobj <- read_xml("~/Downloads/DailyTreasuryYieldCurveRateData.xml")

  # Live script
  xmlobj <- read_xml(yurl)
  
  df <- data.frame( 
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

  df <- df[,c("date", treasury_code)]
  
  # Changing column name to reuse time-series script
  colnames(df) <- c("date", "value")
  
  # time_import$date <- as.numeric(gsub("[T,]", "", nasdaq$value))
  #  as.Date(time_import$date, "%Y-%m-%dT%h:%m:%s")

  # Clips off time and imports value as number
  df$date <- as.Date(df$date)
  df$value <- as.numeric(df$value) 
  
  # Puts into date decreasing order
  df <- df[rev(order(as.Date(df$date), na.last = NA)),]
  
  # Putting into vectors format
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