# covid-monthly-flu.R
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
  library(tidyverse)
  library(bbplot)
  
  #################################################
  # Import, organize and output csv data

  # Input monthly flu data
  df <- read.csv(paste0("~/Documents/R/forecasting/data/flu.csv"), 
                 na.strings = "", skip=0, header=TRUE)
  
  # Assign data types
  df$month <- as.numeric(df$month)
  df$per_100k <- as.numeric(df$per_100k)
  df$year <- as.numeric(df$year)
  
  # The variable used to color and split the data 
  # should be a factor so lines are properly drawn
  #df$month <- factor(df$month, levels = c("3","4","5","6","7",
  #                                      "8","9","10","11",
  #                                      "12","1","2"), ordered=TRUE)
  df$year <- factor(df$year)
  
  plot <- ggplot() + 
    geom_line(data = df, aes(x = month, y = per_100k, color = year)) +
    geom_line(size = 1) +
    # geom_hline(yintercept = 0, size = 1, color="black") +
    # geom_hline(yintercept=as.numeric(800000), linetype=4, colour="black") +
    bbc_style() +
    labs(title=paste0("Influenza Mortality: 
                United States, Per 100,000, ", todays_date),
         subtitle = "") +
    scale_x_discrete(limits=c(3,4,5,6,7,8,9,10,11,12,1,2))
  
  plot
  
  finalise_plot(plot_name = plot,
                source = "Source: Peter Doshi",
                save_filepath = paste0("./output/flu-", todays_date, ".png"),
                width_pixels = 1000,
                height_pixels =1000,
                logo_image_path = paste0("./branding/logo.png"))
  
}
