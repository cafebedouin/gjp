# defun_quick.R 
#################################################
# Description: P

# Replace defaults in function to desired, or 
# call the function from console
defun_quick <- function(file="~/Downloads/UNRATE.csv",
                       trading_days=5, 
                       bin1=3.5, 
                       bin2=4.0, 
                       bin3=4.5, 
                       bin4=5,
                       probability_type="simple",
                       prob_results_title=paste0(code, 
                                                " Probability Table"),
                       # If you want a graph, indicate and add info
                       print_graph="yes",
                       title=paste0(code, " Title "),
                       subtitle="",
                       info_source="Source",
                       file_name="Source",
                       graph_width=1250,
                       graph_height=450) {
  
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
  library(dplyr)
  
  # Sources frequently called forecasting functions
  source("./functions/defun_graph.R")
  source("./functions/defun_simple_probability.R")
  
  #################################################
  # Import, organize and output csv data

  # Import date, value data with header
   df <- read.csv("~/Downloads/UNRATE.csv", 
                     skip=0, header=TRUE)
  
  # Change the names the columns
  colnames(df) <- c("date", "value")
  
  # Reverse dates 
  df <- df[rev(order(df$date)),]   
  
  # Making sure data types are in the correct format
  df$date <- as.Date(df$date)
  df$value <- as.vector(df$value)
  
  #################################################
  # Call desired forecasting functions
  
  if (probability_type == "simple")
    defun_simple_probability(df, prob_results_title,
                             closing_date, trading_days, 
                             bin1, bin2, bin3, bin4)
  
  if (print_graph == "yes")
    defun_graph(df, title, subtitle, info_source, file_name, 
                graph_width, graph_height)
}