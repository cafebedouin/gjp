# covid-us-vaccines-cdc.R
# (c) Scott Jenkins, <srj@posteo.org>
# Original: December 31, 2020
# Last revised: January 6, 2020 

#################################################
# Description:
# Pulls data for shipments of Pfizer & Moderna data
# from CDC, restricts down to second doses

# Clear memory
rm(list=ls())
gc()

covid <- function(url,
                  vaccine,
                  begin_date,
                  fun_date) # End column date + 1
  
  {
  
  #################################################
  # Preliminaries
  
  # Define basepath and set working directory:
  basepath = "~/Documents/R/forecasting"
  setwd(basepath)
  
  # Preventing scientific notation in graphs
  options(scipen=999)
  
  # Set todays_date + 7 because data is ahead
  todays_date <- as.Date(Sys.Date() + 7)
  
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
  library(tidyverse)
  library(bbplot)
  
  #################################################
  # Import, organize and output csv data

  # Downloaded data for testing script 
  #moderna <- read.csv(paste0("/home/scott/Downloads/moderna.csv"), 
  #               na.strings = "", skip=0, header=TRUE)
  
  #pfizer <- read.csv(paste0("/home/scott/Downloads/pfizer.csv"), 
  #               na.strings = "", skip=0, header=TRUE)

  # Live import, switch to testing line when modifying script
  #pfizer <- read.csv("https://data.cdc.gov/resource/saz5-9hgg.csv", 
  #                 na.strings = "", fileEncoding = "UTF-8-BOM")
  #moderna <- read.csv("https://data.cdc.gov/resource/b7pe-5nws.csv", 
  #                 na.strings = "", fileEncoding = "UTF-8-BOM")
  
  df <- read.csv(paste0(url), stringsAsFactors=FALSE, 
                 na.strings = "", skip=0, header=TRUE)
  
  # Remove all rows but total
  df <- df[-c(1:65),]
  
  # Remove total jurisdiction, region and first dose
  df <- df[,-c(1:3)]
  
  # Remove allocation columns
  df <- df %>% select(-contains("allocat"))
  
  # Make long
  df <- reshape(df, 
                direction = "long",
                varying = list(names(df)[1:ncol(df)]))
  
  # Drop id
  df <- df[,-c(3)]
  
  colnames(df) <- c("date", paste0(vaccine)) 
  
  df$date <- seq.Date(as.Date(begin_date), as.Date(fun_date), by = "week")

  return(df)
}

# Set todays_date
todays_date <- Sys.Date()
fun_date <- todays_date + 7

pfizer <- covid(url = "https://data.cdc.gov/resource/saz5-9hgg.csv",
                vaccine = "pfizer",
                begin_date = "2020-12-14",
                fun_date = paste0(fun_date)) 

moderna <- covid(url = "https://data.cdc.gov/resource/b7pe-5nws.csv",
                vaccine = "moderna",
                begin_date = "2020-12-21",
                fun_date = paste0(fun_date)) 

# Strip out commas and change to numeric
pfizer$pfizer <- as.numeric(gsub(",","",pfizer$pfizer))
moderna$moderna <- as.numeric(gsub(",","",moderna$moderna))

# Set todays_date
todays_date <- Sys.Date()

# Fold vaccines into one dataframe
df = merge(x=pfizer, y=moderna, by="date", all=TRUE)

# Change N/A to 0
df[is.na(df)] <- 0

# Create data frame
count <- NULL

# Drop dates from count
count <- df[-c(1)]

# Sum by date
df$total <- rowSums(count)

View(df)

# Drop individual vaccines  
ship <- df[ -c(2:3) ]

View(ship)

# Uncomment for cumulative numbers, comment out this and rbind for weekly shipments
ship[, 2] <- cumsum(ship[, 2])

# Adds last date with projection
#df_proj <- NULL 
#df_proj$date <- as.Date('2021-04-01')
#df_proj$total <- as.numeric('50000000') 
#ship <- rbind(shipm, df_proj)

View(ship)


plot <- ggplot() + 
  geom_line(data = ship, aes(x = date, y = total)) +
  geom_vline(xintercept=todays_date, linetype=1, colour="black") +
  # annotate("text", label = paste0(todays_date), x = todays_date, y = 1000000, size = 10, colour = "black")
  # geom_hline(yintercept=2900000, linetype=3, colour="blue") +
  # geom_hline(yintercept=3300000, linetype=2, colour="red") +
  # geom_hline(yintercept=2500000, linetype=4, colour="black") +
  bbc_style() +
  labs(title=paste0("Vaccine Shipments, ", todays_date),
       subtitle = "") +
  scale_x_date()

plot

finalise_plot(plot_name = plot,
              source = "Source: CDC",
              save_filepath = paste0("./output/covid-us-vaccines-cdc-", todays_date, ".png"),
              width_pixels = 1000,
              height_pixels =500,
              logo_image_path = paste0("./branding/logo.png"))

###############################################
# Printing
# Creating a cvs file of changed data
write.csv(df, file=paste0("./output/covid-us-vaccines-cdc-table-", todays_date, ".csv")) 
