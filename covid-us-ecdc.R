# covid-us-ecdc.R
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
  # df <- read.csv(paste0("~/Downloads/ecdc-data.csv"), 
  #               na.strings = "", fileEncoding = "UTF-8-BOM",
  #               skip=0, header=TRUE)

  # Live import, switch to testing line when modifying script
   df <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                   na.strings = "", fileEncoding = "UTF-8-BOM")
  
  # Drop all the columns but the ones of interest
  df <- df[ -c(2,3,4,5,8,9,10,11) ]
  
  # Set datatypes
  df$dateRep <- as.Date(df$dateRep, format="%d/%m/%Y")
  df$deaths <- as.numeric(df$deaths) 

  # Sort by date
  df <- df[order(as.Date(df$dateRep)),]
  
  # Rename dateRep to date
  colnames(df)[1] <- "date"
  colnames(df)[3] <- "country"
  
  # Create data frame
  us <- NULL
  
  # Reshape data, date rows and country columns 
  us <- reshape(df, direction="wide", idvar="date", timevar="country")
  
  # Fix the names on the columns
  names(us) <- gsub("deaths.", "", names(us))
  
  us[is.na(us)] <- 0

  # Drop dates from count
  count  <- us[ -c(1) ]

  # Sum by date
  us$us <- rowSums(count)
  
  # Check if number of columns has changed
  # View(us)
  # }  

  # US only
  us <- us[ c(1,67) ]
  
  # Uncomment for cumulative numbers, comment out for daily deaths
  # us[, 2] <- cumsum(us[, 2])
  
  # Add in end date and value 
  
  # df_add <- read.csv(paste0("~/Downloads/us-end.csv"), 
  #                   skip=0, header=TRUE)
  
  # us <- rbind(us, df_add)
  
  View(us)

  plot <- ggplot() + 
    geom_line(data = us, aes(x = date, y = United_States_of_America)) +
  #  geom_vline(xintercept=todays_date, linetype=4, colour="black") +
    geom_vline(yintercept=2143, linetype=4, colour="blue") +
    bbc_style() +
    labs(title=paste0("United States Deaths, ", todays_date),
         subtitle = "") +
    scale_x_date()
  
  plot
  
  finalise_plot(plot_name = plot,
                source = "Source: ECDC",
                save_filepath = paste0("./output/covid-us-ecdc-", todays_date, ".png"),
                width_pixels = 1000,
                height_pixels =1000,
                logo_image_path = paste0("./branding/logo.png"))
  
  ###############################################
  # Printing
  # Creating a cvs file of changed data
  write.csv(us, file=paste0("./output/ecdc-us-table-", todays_date, ".csv")) 
}
