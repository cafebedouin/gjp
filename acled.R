# acled.R
# (c) Scott Jenkins, <srj@posteo.org>, July 9, 2020
# Adapted from: 
# https://github.com/jeremy-allen/getACLED/blob/master/acled_api_pull.R
# Last revised: July 9, 2020 

#################################################
# Description: Pulls data from ACLED by country 

# Clear memory
rm(list=ls())
gc()

acled <- function(country="Ethiopia",
                  event="Riots",
                  year=NULL) {
  
  # Events can include: NULL (includes all), "Battles", "Violence against civilians", 
  # "Explosions/Remote violence", "Riots, "Protests", "Strategic developments" 
  
  
  ###################################################
  # Preliminaries
  
  #Preventing scientific notation in graphs
  options(scipen=999)

  #Load libraries for analysis
  #If library X is not installed, you can install it with this command:
  # install.packages('X')
  library(jsonlite)
  library(data.table)
  library(lubridate)
  library(stringr)
  library(ggplot2)
  library(plyr)
  library(RCurl)
  
  #Replace spaces in country name with space code:
  country <- str_replace_all(country, " ", "%20")

  ###############################################
  # ACLED DATA FUNCTION
  
  aurl <- "https://api.acleddata.com/acled/read?&terms=accept&limit=10000"
  
    #Define API url:
    if(is.null(country) & is.null(year)) {
      aurl <- "https://api.acleddata.com/acled/read?&terms=accept&limit=10000"
    } else if (!is.null(country) & is.null(year)) {
      aurl <- paste0(aurl, "&country=", country, "&country_where=%3D", "&limit=0")
    } else if (is.null(country) & !is.null(year)) {
      aurl <- paste0(aurl, "&year=", year, "&limit=0")
    } else {
      aurl <- paste0(aurl, "&country=", country, "&country_where=%3D", "&year=", year, "&limit=0")
    }

    # Download the data! The API returns a list:
    df <- fromJSON(aurl) 
    
    #Select just the data from the returned list:
    df <- as.data.table(df[[5]])
  
    #Convert event_date column to date class:
    df[, event_date := ymd(event_date)]
  
    #Convert latitude column to double(numeric) class:
    df[, latitude := as.double(latitude)]
  
    #Convert longitude column to double(numeric) class:
    df[, longitude := as.double(longitude)]
  
    #Convert fatalities column to double(numeric) class:
    df[, fatalities := as.numeric(fatalities)]
  
    #Define the month and the year of each event:
    df$month = as.numeric(substr(df$event_date,6,7))
    df$year = as.numeric(df$year)
  
    # Limit to since 2013
    df <- df[which(df$year>=2013),]
    
    View(df)
}
    
    df <- df[which(df$event_type==paste0(event)),]
    
    ###############################################
    # EVENT ANALYSIS:
    
    # Identify events, summarize at the month level, and rename variables:
    events <- ddply(df, .(df$year, df$month), nrow)
    names(events) = c("year","month","frequency")

    todays_date <- Sys.Date()
    events_year <- as.numeric(gsub("-..-..", "", todays_date))
    events_month <- gsub("....-", "", todays_date)
    events_month <- as.numeric(gsub("-..", "", events_month))
  
  #Add observations for months with no events, and fill in zeroes:
  for (yearval in 2013:events_year){
    for (monthval in 1:12){
      if (nrow(events[which(events$year==yearval & events$month==monthval),])==0 &
          (yearval<events_year | monthval<=events_month)){
      
            events = rbind(events,c(yearval,monthval,0))
      }
    }
  }

  #Check for seasonality with a linear regression:
  summary(lm(frequency~as.factor(month)+as.factor(year),data=events))

  #How many months had a riot/protest?
  event_odds <- as.data.frame(table(events$frequency>=1))
  event_yes <- event_odds[2,2]
  event_no <- event_odds[1,2]
  event_total_months <- event_yes + event_no
  event_probability <- event_yes / event_total_months
  events = events[with(events, order(year, month)), ]
  
  # Creating a cvs file of riot data
  write.csv(events, file=paste0("./", event, "-in-", country, "-table-",
                                   todays_date, ".csv")) 
  
  return(cat(paste0("Historical Probability \n",
                    "Consult events-in-", country, 
                    " csv file to check base rate against recent trends.\n\n",
                    "Months event occured in ", country, " / ",
                    "possible months events could occur = ",
                    "historical possiblity of ", event,  ". \n",
                    event_yes, " / ", event_total_months, " = ", 
                    event_probability, " chance of a ", event, ". \n")))
}
