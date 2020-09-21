# covid-africa-cases-ecdc.R
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
  # df <- read.csv(paste0("/home/scott/Downloads/ecdc-data.csv"), 
  #               na.strings = "", fileEncoding = "UTF-8-BOM")
  #               skip=0, header=TRUE)

  # Live import, switch to testing line when modifying script
  df <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                   na.strings = "", fileEncoding = "UTF-8-BOM")
 
  df <- filter(df, continentExp == "Africa")

  # Drop all the columns but the ones of interest
  # Keep either 5 for cases or 6 for deaths 
  df <- df[ -c(2,3,4,6,8,9,10,11,12) ]
  
  # Set datatypes
  df$dateRep <- as.Date(df$dateRep, format="%d/%m/%Y")
  df$cases <- as.numeric(df$cases) 
  
  # Sort by date
  df <- df[order(as.Date(df$dateRep)),]
  
  # Rename dateRep to date
  colnames(df)[1] <- "date"
  colnames(df)[3] <- "country"
  
  # Create data frame
  africa <- NULL
  
  # Reshape data, date rows and country columns 
  africa <- reshape(df, direction="wide", idvar="date", timevar="country")
  
  # Fix the names on the columns
  names(africa) <- gsub("cases.", "", names(africa))
  
  africa[is.na(africa)] <- 0

  # Drop dates from count
  count  <- africa[ -c(1) ]

  # Sum by date
  africa$total <- rowSums(count)
  
  # Check if number of columns has changed
  # View(africa)

  africa <- africa[ -c(2:56) ]
  
  # Uncomment for cumulative numbers, comment out for daily
  africa[, 2] <- cumsum(africa[, 2])
  
  df_add <- read.csv(paste0("~/Downloads/africa-end.csv"), 
                     skip=0, header=TRUE)
  
  africa <- rbind(africa, df_add)
  
  plot <- ggplot() + 
    geom_line(data = africa, aes(x = date, y = total)) +
    geom_vline(xintercept=todays_date, linetype=4, colour="black") +
    bbc_style() +
    labs(title=paste0("Africa Cases, ", todays_date),
         subtitle = "") +
    scale_x_date()
  
  plot
  
  finalise_plot(plot_name = plot,
                source = "Source: ECDC",
                save_filepath = paste0("./output/covid-africa-ecdc-", todays_date, ".png"),
                width_pixels = 750,
                height_pixels =500,
                logo_image_path = paste0("./branding/logo.png"))
  
  ###############################################
  # Printing
  # Creating a cvs file of changed data
  write.csv(africa, file=paste0("./output/ecdc-africa-table-", todays_date, ".csv")) 
}
