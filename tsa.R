# tsa.R 
#################################################
# Description: Imports csv file and has defaults
# for removing columns, rows, filtering on 
# content and putting the last date/value pairs
# in the appropriate data types.

# Replace defaults in function to desired, or 
# call the function from console
csv <- function(begin_date="2008-01-01", # For analysis, not question
                closing_date="2021-07-01",
                freq="daily",
                trading_days=7, 
                bin1=25000, 
                bin2=50000, 
                bin3=75000, 
                bin4=100000,
                probability_type="monte",
                annual_percent="no",
                prob_results_title="BTC",
                 # If you want a graph, indicate and add info
                graph="no",
                title=paste0(" TSA "),
                subtitle="",
                info_source="TSA",
                file_name="TSA",
                graph_width=1750,
                graph_height=450,
                df_summary="yes") {
  
  #################################################
  # Preliminaries
  
  # Preventing scientific notation in graphs
  options(scipen=999)
  
  # Set todays_date
  todays_date <- Sys.Date()
  
  # Set date for ten years ago
  ten_years <- todays_date - 3652
  
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
  df <- read.csv(paste0("./data/TSA.csv"), 
                        skip=0, header=TRUE)
  
  # Names the columns
  colnames(df) <- c("date", "passengers")
  
  df$date <- as.Date(df$date)
  df$passengers <- as.numeric(df$passengers) 
  View(df)
  
  # Adds last date with projection
  #df_proj <- NULL 
  #df_proj$date <- as.Date('2021-08-01')
  #df_proj$passengers <- as.numeric('2300000') 
  #df <- rbind(df, df_proj)
  
  # Filter out days with value of "."
  # df <- filter(df, value != ".") 
  # df %>% is.na()
  
  fit <- lm(formula = date~passengers, data = df)
  View(fit)
  
  plot <- ggplot() + 
    ggtitle(paste("TSA Passengers")) +
    geom_line(data = df, aes(x = date, y = passengers)) + 
    # geom_line(data = fit)
    geom_hline(yintercept=2300000, color="red") +
    scale_x_date()
  
  plot
  
  finalise_plot(plot_name = plot,
                source = "Source: TSA",
                save_filepath = paste0("./output/tsa-passengers-",
                                       todays_date, ".png"),
                width_pixels = 1500,
                height_pixels =500,
                logo_image_path = paste0("./branding/logo.png"))
  
}
