# covid-africa-tests-owid.R
# (c) Scott Jenkins, <srj@posteo.org>
# Original: October 1, 2020
# Last revised: October 31, 2020 

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
  library(dplyr)
  library(plyr)
  library(readr)
  library(ggplot2)
  library(ggpubr)
  # library(tidyverse)
  library(bbplot)
  
  #################################################
  # Import, organize and output csv data

  # Download data for testing script 
  #df <- read.csv(paste0("/home/scott/Downloads/owid-covid-data.csv"), 
  #               na.strings = "", skip=0, header=TRUE)

  # Live import, switch to testing line when modifying script
  df <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv", 
                   na.strings = "", fileEncoding = "UTF-8-BOM")
 
  df <- filter(df, continent == "Africa")

  # Drop all the columns but the ones of interest
  # Cumulative tests is 26, daily tests is 27
  df <- df[ -c(1,2,5:25,27:52) ]

  # View to see column numbers and troubleshoot
  #View(df)
  #}
  
  # Rename dateRep to date
  colnames(df)[1] <- "country"
  colnames(df)[2] <- "date"
  colnames(df)[3] <- "tests"  
  
  # Set datatypes
  df$date <- as.Date(df$date)
  df$tests <- as.numeric(df$tests) 
  
  # Sort by date
  df <- df[order(as.Date(df$date)),]
  
  # Rename dateRep to date
  colnames(df)[1] <- "country"
  colnames(df)[2] <- "date"
  
  # Create data frame
  africa <- NULL
  
  # Reshape data, date rows and country columns 
  africa <- reshape(df, direction="wide", idvar="date", timevar="country")
  
  # Fix the names on the columns
  names(africa) <- gsub("tests.", "", names(africa))

  # View(africa)
  
  africa[is.na(africa)] <- 0

  # Drop dates from count
  count  <- africa[ -c(1) ]

  # Sum by date
  africa$total <- rowSums(count)
  
  # Check if number of columns has changed
  # View(africa)

  africa <- africa[ -c(2:55) ]
  
  # Uncomment for cumulative numbers, comment out for daily
  # If commented out, also comment out df_proj below
  africa[, 2] <- cumsum(africa[, 2])
  
  # Clips off last 5 days to get rid of incomplete data 
  africa <- head(africa,-5)
  # todays_date_sub5 <- paste0(as.Date(todays_date)-5)
  
  # Adds last date with projection
  df_proj <- NULL 
  df_proj$date <- as.Date('2021-04-01')
  df_proj$total <- as.numeric('50000000') 
  africa <- rbind(africa, df_proj)

  View(africa)
    
  plot <- ggplot() + 
    geom_line(data = africa, aes(x = date, y = total)) +
    geom_vline(xintercept=todays_date-5, linetype=4, colour="black") +
    bbc_style() +
    labs(title=paste0("Africa Cumulative Tests, ", todays_date-5),
         subtitle = "")
    scale_x_date()
  
  plot
  
  finalise_plot(plot_name = plot,
                source = "Source: OWID",
                save_filepath = paste0("./output/covid-africa-cum-owid-", todays_date-5, ".png"),
                width_pixels = 750,
                height_pixels =500,
                logo_image_path = paste0("./branding/logo.png"))
  
  ###############################################
  # Printing
  # Creating a cvs file of changed data
  write.csv(africa, file=paste0("./output/covid-africa-owid-table-tests-", todays_date, ".csv")) 
}
