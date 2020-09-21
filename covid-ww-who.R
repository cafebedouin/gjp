# covid-ww-who.R
# (c) Scott Jenkins, <srj@posteo.org>
# Original: April 15, 2020
# Last revised: April 15, 2018 

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

covid <- function(country = "United States",
                  end_date = "2020-04-25",
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
                 skip=0, header=FALSE)

  # Live import, switch to testing line when modifying script
  # df <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
  #                 na.strings = "", fileEncoding = "UTF-8-BOM")

  # Sort by date column
  df <- df[order(as.Date(df$V1)),]
  
  # Drop all the columns but the ones of interest
  df <- df[ -c(2,4) ] 
  
  # Add column names
  colnames(df)[1] <- "date"
  colnames(df)[2] <- "country"
  colnames(df)[3] <- "new_deaths"
  colnames(df)[4] <- "deaths"
  colnames(df)[5] <- "new_cases"
  colnames(df)[6] <- "cases"
  
  # Set datatypes
  df$date <- as.Date(df$date) 
  df$country <- as.character(df$country)
  df$new_deaths <- as.numeric(df$new_deaths) 
  df$deaths <- as.numeric(df$deaths)   
  df$new_cases <- as.numeric(df$new_cases) 
  df$cases <- as.numeric(df$cases) 
  
  # Limit by data of interest
  df <- filter(df, country == "United States of America")
#  df <- filter(df, date > "2020-03-15")
  
  # Drop all the columns but the ones of interest
  df <- df[ -c(2,3,5,6) ] 
  
  View(df)
  
  plot <- ggplot() + 
    geom_line(data = df, aes(x = date, y = deaths)) +
#    geom_vline(xintercept=as.numeric(as.Date(todays_date-1)), linetype=4, colour="black") +
    bbc_style() +
    labs(title=paste0("Deaths in ", country, ", ", todays_date),
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
  
}

