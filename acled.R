# acled_riots.R
# (c) Ezra Karger, May 14, 2018
# Modified by Scott Jenkins, <srj@posteo.org>
# Last revised: December 4, 2018 

#################################################
# Description: Pulls data from ACLED by country 
# for battle, creates csv and makes prediction

#Clear memory
rm(list=ls())
gc()

acled_riots <- function(country="Gabon") {

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

  ###############################################
  # ACLED DATA FUNCTION

  #This is adapted from https://github.com/jeremy-allen/getACLED/blob/master/acled_api_pull.R
  getACLED <- function(country = "Ethiopia", year = NULL) {
    
    #This is the base URL for the api:
    myurl <- "https://api.acleddata.com/acled/read?&terms=accept&"
  
    #Replace spaces in country name with space code:
    country2 <- str_replace_all(country, " ", "%20")
  
    #Define API call, which depends on whether you are searching for
    # a country or a year:
    if(is.null(country2) & is.null(year)) {
      myurl <- "https://api.acleddata.com/acled/read?&terms=accept&limit=10000"
    } else if (!is.null(country2) & is.null(year)) {
      myurl <- paste0(myurl, "&country=", country2, "&country_where=%3D", "&limit=0")
    } else if (is.null(country2) & !is.null(year)) {
      myurl <- paste0(myurl, "&year=", year, "&limit=0")
    } else {
      myurl <- paste0(myurl, "&country=", country2, "&country_where=%3D", "&year=", year, "&limit=0")
    }

  
    # Download the data! The API returns a list:
    acledEvents <- fromJSON(myurl)
    
    View(acledEvents)
  }
}

  
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
    acledEvents = acledEvents[which(acledEvents$year>=2011),]
  
    return(acledEvents)
  }

  ###############################################
  #RIOT/PROTEST ANALYSIS:

  #Download all events for country
  data_riots <- getACLED()

  #Identify riots, summarize at the month level, and rename variables:
  data_riots_hit = data_riots[which(data_riots$event_type == "Riots/Protests"),]
  counts_riots <- ddply(data_riots_hit, .(data_riots_hit$year, data_riots_hit$month), nrow)
  names(counts_riots) = c("year","month","frequency")

  riot_date <- Sys.Date()
  riot_year <- as.numeric(gsub("-..-..", "", riot_date))
  riot_month <- gsub("....-", "", riot_date)
  riot_month <- as.numeric(gsub("-..", "", riot_month))
  
  #Add observations for months with no events, and fill in zeroes:
  for (yearval in 2010:riot_year){
    for (monthval in 1:12){
    
      if (nrow(counts_riots[which(counts_riots$year==yearval & counts_riots$month==monthval),])==0 &
          (yearval<riot_year | monthval<=riot_month)){
      
            counts_riots = rbind(counts_riots,c(yearval,monthval,0))
      }
    }
  }

  #Check for seasonality with a linear regression:
  summary(lm(frequency~as.factor(month)+as.factor(year),data=counts_riots))

  #How many months had a riot/protest?
  riot_odds <- as.data.frame(table(counts_riots$frequency>=1))
  riot_yes <- riot_odds[2,2]
  riot_no <- riot_odds[1,2]
  riot_total_months <- riot_yes + riot_no
  riot_probability <- riot_yes / riot_total_months
  counts_riots = counts_riots[with(counts_riots, order(year, month)), ]
  
  # Creating a cvs file of riot data
  write.csv(counts_riots, file=paste0("./output/riots-in-", country, "-table-",
                                   riot_date, ".csv")) 
  
  return(cat(paste0("Historical Probability \n",
                    "Consult riots-in-", country, 
                    " csv file to check base rate against recent trends.\n\n",
                    "Months riot occured in ", country, " / ",
                    "possible months riots could occur = ",
                    "historical possiblity of riot. \n",
                    riot_yes, " / ", riot_total_months, " = ", 
                    riot_probability, " chance of a riot. \n")))
}
