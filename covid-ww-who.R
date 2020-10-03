# covid-ww-who.R
# cafebedouin@gmail.com, 2020
#################################################
# Description:
# Script parses cvs file and provides a graph 
# with selectable yearly trend lines for 
# comparison, also includes analysis options at 
# bottom for predicting a particular day or 
# searching by cases.

# Clear memory
rm(list=ls())
gc()

covid <- function(country = "",
                  end_date = "2021-04-01",
                  end_value = "600") {
  
  #################################################
  # Preliminaries
  
  # Define basepath and set working directory:
  basepath = "~/Documents/R/forecasting"
  setwd(basepath)
  
  # Preventing scientific notation in graphs
  options(scipen=999)
  
  # Set todays_date
  todays_date <- Sys.Date()
  
  #################################################
  # Load libraries. If library X is not installed
  # you can install it with this command at the R prompt:
  # install.packages('X')
  library(data.table)
  library(lubridate)
  library(dplyr)
  library(plyr)
  library(tidyverse)
  library(bbplot)
  
  #################################################
  # Import, organize and output csv data

  # Download data for testing script 
  df <- read.csv(paste0("~/Downloads/WHO-COVID-19-global-data.csv"), 
                 na.strings = "", fileEncoding = "UTF-8-BOM",
                 skip=0, header=TRUE)

  # Live import, switch to testing line when modifying script
  #df <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv", 
  #                 na.strings = "", header=TRUE, fileEncoding = "UTF-8-BOM")
  
  # Drop all the columns but the ones of interest
  df <- df[ -c(2,4:6,8) ] 
  
  # Add column names
  colnames(df)[1] <- "date"
  colnames(df)[2] <- "country"
  colnames(df)[3] <- "deaths"
  
  # Set datatypes  
  df$date <- as.Date(df$date, origin="1970-01-01")
  df$country <- as.character(df$country)
  df$deaths <- as.numeric(df$deaths) 
  
  # Sort by date column
  df <- df[order(as.Date(df$date)),]
  
  # Limit by data of interest
  # df <- filter(df, country == "United States of America")
  # df <- filter(df, date > "2020-03-15")
  
  # Create data frame
  ww <- NULL
  
  # Reshape data, date rows and country columns 
  ww <- reshape(df, direction="wide", idvar="date", timevar="country")
  
  # Fix the names on the columns
  names(ww) <- gsub("deaths.", "", names(ww))
  
  ww[is.na(ww)] <- 0
  
  # Drop dates from count
  count  <- ww[ -c(1) ]
  
  # Sum by date
  ww$ww <- rowSums(count)
  
  # ww only
  # ww <- ww[ c(1,67) ]
  
  # Check if number of columns has changed
  # View(ww)

  # ww only
  ww <- ww[ -c(2:236) ]
  
  # Uncomment for cumulative numbers, comment out for daily deaths
  # ww[, 2] <- cumsum(ww[, 2])
  
  # Add in end date and value 
  
  # df_add <- read.csv(paste0("~/Downloads/ww-end.csv"), 
  #                   skip=0, header=TRUE)
  
  # ww <- rbind(ww, df_add)
  
  plot <- ggplot() + 
    geom_line(data = df, aes(x = date, y = deaths)) +
#    geom_vline(xintercept=as.numeric(as.Date(todays_date-1)), linetype=4, colour="black") +
    bbc_style() +
    labs(title=paste0("New Deaths Worldwide, ", todays_date),
         subtitle = "None") +
    scale_x_date()
  
  plot
  
  finalise_plot(plot_name = plot,
                source = "Source: WHO",
                save_filepath = paste0("./output/covid-ww-who-",
                                       todays_date, ".png"),
                width_pixels = 1000,
                height_pixels =1000,
                logo_image_path = paste0("./branding/logo.png"))
  
  ###############################################
  # Printing
  # Creating a cvs file of changed data
  write.csv(ww, file=paste0("./output/covid-ww-who-new-deaths-", todays_date, ".csv")) 
}
  
