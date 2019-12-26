# acled_battle.R
#################################################
# Description: Pulls data from ACLED by country 
# for battle, creates csv and makes prediction

#STEP 1: PRELIMINARIES

#Clear memory
rm(list=ls())
gc()

acled_battle <- function() {

  # Define basepath and set working directory:
  basepath = "~/Documents/programs/R/forecasting"
  setwd(basepath)
  
  # Set time
  acled_date <- Sys.Date()

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

  ###############################################
  #BATTLE DEATH ANALYSIS

  #Replace Yemen with country of interest. Download all country events:
  data_battle <- getACLED("Yemen")

  #Subset to relevant events:
  data_battle2 = data_battle[which(data_battle$event_type=="Battle-Government regains territory" |
                                  data_battle$event_type=="Battle-No change of territory"|
                                  data_battle$event_type=="Battle-Non-state actor overtakes territory"),]

  #Summarize to the monthly and daily level for analysis:
  fatalities_battle <- ddply(data_battle2, .(year, month), summarise,sum=sum(fatalities))
  fatalities_battle_daily <- ddply(data_battle2, .(event_date), summarise,sum=sum(fatalities))

  # Make a .csv file
  write.csv(fatalities_battle, paste0("./output/acled-battle-in-", 
                                      country, "-from-", acled_date, "-monthly.csv"))
  write.csv(fatalities_battle_daily, paste0("./output/acled-battle-in-", 
                                      country, "-from-", acled_date, "-daily.csv"))

  #Subset to 2018 (or relevant year) events and create a local linear regression best-fit line:
  temp = fatalities_battle_daily[which(substr(fatalities_battle_daily$event_date,1,4)=="2018"),]
  temp$index <- 1:nrow(temp)
  loessMod25 <- loess(sum ~ index, data=temp, span=0.25) # 25% smoothing span
  smoothed25 <- predict(loessMod25) 

  #Create a PDF with a figure for daily battle deaths in country in current year:
  pdf(paste0("./output/acled-battle-in-", country, "-from-", acled_date, "-daily-loess.pdf"))
  plot(temp$sum, x=temp$event_date, type="l", main="Loess Smoothing and Prediction", xlab="Date", ylab="Daily Fatalities")
  lines(smoothed25, x=temp$event_date, col="green")
  dev.off()

  return(fatalities_battle[nrow(fatalities_battle),])
}
