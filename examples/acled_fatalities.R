# acled_fatalities.R
#################################################
# Description:
# Script provides a ratio of how many months had x_fatalities

# Clear memory
rm(list=ls())
gc()

#################################################
# Set Variables in Function
acled_fatalities <- function(country="Yemen", x_fatalities=1) {
 
  # Preliminaries
  basepath = "~/Documents/programs/R/forecasting"
  setwd(basepath)

  #Preventing scientific notation in graphs
  options(scipen=999)

  # Load libraries for analysis
  # If library X is not installed, you can install it with this command:
  # install.packages('X')
  library(jsonlite)
  library(data.table)
  library(lubridate)
  library(stringr)
  library(ggplot2)
  library(plyr)
  library(RCurl)

  ###############################################
  # DEFINE FUNCTION TO READ IN ACLED DATA:

  #This is adapted from https://github.com/jeremy-allen/getACLED/blob/master/acled_api_pull.R
  getACLED <- function(country = NULL, year = NULL) {
      
    #This is the base URL for the api:
    myurl <- "https://api.acleddata.com/acled/read"
      
    #Replace spaces in country name with space code:
    country2 <- str_replace_all(country, " ", "%20")
      
    # Define API call, which depends on whether you are searching for
    # a country or a year:
    if(is.null(country2) & is.null(year)) {
      myurl <- "https://api.acleddata.com/acled/read?&limit=10000"
    } else if (!is.null(country2) & is.null(year)) {
      myurl <- paste0(myurl, "&country=", country2, "&country_where=%3D", "&limit=0")
    } else if (is.null(country2) & !is.null(year)) {
      myurl <- paste0(myurl, "&year=", year, "&limit=0")
    } else {
      myurl <- paste0(myurl, "&country=", country2, "&country_where=%3D", "&year=", year, "&limit=0")
    }
    
    #Download the data! The API returns a list:
    acledEvents <- fromJSON(myurl)
      
    #Select just the data from the returned list:
    acledEvents <- as.data.table(acledEvents[[4]])
    View(acledEvents)
      
    #Drop 'data.' from all column names:
    names(acledEvents) <- str_replace_all(names(acledEvents), 'data.', '')
      
    #Convert event_date column to date class:
    acledEvents[, event_date := ymd(event_date)]
      
    #Convert latitude column to double(numeric) class:
    acledEvents[, latitude := as.double(latitude)]
      
    #Convert longitude column to double(numeric) class:
    acledEvents[, longitude := as.double(longitude)]
      
    #Convert fatalities column to double(numeric) class:
    acledEvents[, fatalities := as.numeric(fatalities)]
      
    #Define the month and the year of each event:
    acledEvents$month = as.numeric(substr(acledEvents$event_date,6,7))
    acledEvents$year = as.numeric(acledEvents$year)
      
    #Restrict data to observations post 2009 (I don't trust the pre-2010 data)
    acledEvents = acledEvents[which(acledEvents$year>=2010),]
      
    return(acledEvents)
  }

  # Request all events from ACLED and subset to events with 15+ fatalities
  data_fatal = getACLED(country)

  # Identify fatalities, summarize at the month level, and rename variables:                           
  data_fatal_hit = data_fatal[which(data_fatal$month==4&fatalities>=x_fatalities),]                 
  counts_fatal <- ddply(data_fatal_hit, .(data_fatal_hit$year, data_fatal_hit$month), nrow)      
  names(counts_fatal) = c("year","month","frequency")                                            
                                                                                               
  # Add observations for months with no events, and fill in zeroes:                               
  for (yearval in 2011:2019){                                                                    
    for (monthval in 1:12){                                                                      
      if (nrow(counts_fatal[which(counts_fatal$year==yearval & 
                                  counts_fatal$month==monthval),])==
          (yearval<=2019 | monthval<=12)){                  
            counts_fatal = rbind(counts_fatal,c(yearval,monthval,0))                             
      }
    }
  }
                         
  # How many months had an incident with x or more fatalities?                                                           
  table(counts_fatal$frequency>=x_fatalities)
  counts_fatal = counts_fatal[with(counts_fatal, order(year, month)), ] 

  return(myurl)
}
