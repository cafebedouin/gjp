# covid-us-states.R
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

covid <- function() {
  
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
  library(tidyverse)
  library(bbplot)
  
  #################################################
  # Import, organize and output csv data

  # Download data for testing script 
  # df <- read.csv(paste0("~/Downloads/us-states.csv"), 
  #                   skip=0, header=TRUE)

  # Live import, switch to testing line when modifying script
  nurl = paste0("https://raw.githubusercontent.com/",
                "nytimes/covid-19-data/master/us-states.csv")
  
  df <- read.csv(nurl, skip=0, header=TRUE)
  
  df_add <- read.csv(paste0("~/Downloads/us-states-end.csv"), 
                     skip=0, header=TRUE)
  
  df <- rbind(df, df_add)
  
  # Drop all the columns but the ones of interest
  df <- df[ -c(3,4) ]
  
  # Set datatypes
  df$date <- as.Date(df$date, origin = "2020-03-15")
  df$state <- as.character(df$state)
  df$deaths <- as.numeric(df$deaths) 
  
  df[is.na(df$date)] <- "2020-05-01"
  
  df <- filter(df, state == "Maryland" | 
                   state == "Washington" |
                   state == "Indiana" | 
                   state == "Illinois" | 
                   state == "Ohio" | 
                   state == "Texas" | 
                   state == "Colorado" | 
                   state == "Massachusetts" | 
                   state == "Georgia" | 
                   state == "Florida" | 
                   state == "California" |
                   state == "Connecticut" |
                   state == "Pennsylvania")
  
  df <- filter(df, date > "2020-03-01")  
  
  plot <- ggplot() + 
    geom_line(data = df, aes(x = date, y = deaths, color = state)) +
    geom_vline(xintercept=as.numeric(as.Date(todays_date-1)), linetype=4, colour="black") +
    bbc_style() +
    labs(title=paste0("Deaths by State, ", todays_date),
         subtitle = "From 1-2 Days Before Date Run to 1,000 on May 1st ") +
    scale_x_date()

  plot
  
  finalise_plot(plot_name = plot,
                source = "Source: The New York Times",
                save_filepath = paste0("./output/covid-us-states-", todays_date, ".png"),
                width_pixels = 1000,
                height_pixels = 750,
                logo_image_path = paste0("./branding/logo.png"))

  }
