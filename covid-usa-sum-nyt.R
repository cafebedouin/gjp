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
  
  # Drop all the columns but the ones of interest
  df <- df[ -c(3,4) ]
  
  # Set datatypes
  df$date <- as.Date(df$date)
  df$deaths <- as.numeric(df$deaths) 
  
  # Sort by date
  df <- df[order(as.Date(df$date)),]
  colnames(df)[2] <- "state"
  
  # Create data frame
  usa <- NULL
  
  # Reshape data, date rows and country columns 
  usa <- reshape(df, direction="wide", idvar="date", timevar="state")
  
  # Fix the names on the columns
  names(usa) <- gsub("deaths.", "", names(usa))
  
  usa[is.na(usa)] <- 0
  
  # Drop dates from count
  count  <- usa[ -c(1) ]
  
  # Sum by date
  usa$usa <- rowSums(count)
  
  # USA only
  usa <- usa[ -c(2:56) ]
  
  View(usa)
  
  # Uncomment for cumulative numbers, comment out for daily deaths
  # ww[, 2] <- cumsum(ww[, 2])
  
  plot <- ggplot() + 
    geom_line(data = usa, aes(x = date, y = usa)) +
    geom_vline(xintercept=todays_date, linetype=4, colour="black") +
    bbc_style() +
    labs(title=paste0("U.S. Deaths, ", todays_date),
         subtitle = "") +
    scale_x_date()
  
  plot
  
  finalise_plot(plot_name = plot,
                source = "Source: NYT",
                save_filepath = paste0("./output/covid-usa-sum-nyt-", todays_date, ".png"),
                width_pixels = 1000,
                height_pixels =1000,
                logo_image_path = paste0("./branding/logo.png"))
  
  ###############################################
  # Printing
  # Creating a cvs file of changed data
  write.csv(ww, file=paste0("./output/covid-usa-sum-nyt-", todays_date, ".csv"))
  
}
