# acled_civilian.R
#################################################
# Description: Pulls ACLED Civilian attack data 
# from 2010-present and outputs csv

# Clear memory
rm(list=ls())
gc()

#################################################
# Set Variables in Function
acled_civilian <- function(country="Burundi", x_civilian=1) {

  basepath = "~/Documents/programs/R/forecasting"
  setwd(basepath)

  #Preventing scientific notation in graphs
  options(scipen=999)

  # Set time
  acled_date <- Sys.Date()
  
  #Load libraries for analysis
  #If library X is not installed, you can install it with this command:
  # install.packages('X')
  library(jsonlite)
  library(data.table)
  library(lubridate)
  library(stringr)
  library(plyr)
  library(dplyr)

  #STEP 2: DEFINE FUNCTION TO READ IN ACLED DATA:

  #This is adapted from https://github.com/jeremy-allen/getACLED/blob/master/acled_api_pull.R
  getACLED <- function(country = NULL, year = NULL) {
    
    #This is the base URL for the api:
    myurl <- "https://api.acleddata.com/acled/read?"
  
    #Replace spaces in country name with space code:
    country2 <- str_replace_all(country, " ", "%20")
  
    #Define API call, which depends on whether you are searching for
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
    acledEvents = acledEvents[which(acledEvents$year>=2015),]
  
    return(acledEvents)
  }

  ###############################################
  # CIVILIAN FATALITIES BY COUNTRY
  acled_civ <- getACLED(country)

  # Pull the relevant row data that has more than x fatalities and matches event type/actor
  acled_civ2 = acled_civ[which(acled_civ$fatalities>=x_civilian & 
                               (acled_civ$event_type=="Violence against civilians" |
                                    (acled_civ$event_type=="Remote violence" &
                                    grepl("Civilians",acled_civ$actor2)))),]

  acled_civ2$fatalities=1
  fatalities_civ <- ddply(acled_civ2, .(year, month), summarise, sum=sum(fatalities))
  write.csv(fatalities_civ, paste0("./output/acled-fatalities-in-", 
                                 country, "-", acled_date, ".csv")) 

  return(fatalities_civ)
  #  return(cat(paste0(fatalities_civ[nrow(fatalities_civ),3])))
}