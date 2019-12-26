# graph_fun.R 
#################################################
# Description: Generates a graph from time series
# using BBC style: https://bbc.github.io/rcookbook/
# See url for initial set-up.

rm(list=ls()) # Clear memory
gc()

# Replace defaults in function to desired, or call the function from console
graph_fun <- function(file_path="./data/pce-q.csv") {
  
  #################################################
  # Load libraries. If library X is not installed
  # you can install it with this command at the R prompt:
  # install.packages('X') 
  #
  # Also: install dev.tools then in the console:
  # devtools::install_github('bbc/bbplot')
  
  library(dplyr)
  library(tidyr)
  library(gapminder)
  library(ggplot2)
  library(forcats)
  library(png)
  library(grid)
  library(ggpubr)
  library(scales)
  library(bbplot)
  
  # Hold date to add to file output
  run_date <- Sys.Date()
  
  data_import <- read.csv(file_path, header=FALSE) 

  # Adding column names, changing sea name to sea ice
  colnames(data_import) <- c("Quarter", "Value")

  # Check input
  #View(data_import) } 
  
  #################################################
  # Historic Trends Graph
  graph_df <- data_import
  
  # These variables need to be numerical
  # and changes date to ordered number
  graph_df$Quarter <- as.numeric(graph_df$Quarter)
  graph_df$Value <- as.numeric(graph_df$Value)
  
  # Check input
  # View(graph_df) }
  
  # Filtering results into period of interest
  # graph_df <- filter(graph_df, Day <= days_end)  
  # graph_df <- filter(graph_df, Day >= days_start)
  
  #Make plot
  line <- ggplot(graph_df, aes(x = Quarter, y = Value)) +
    geom_point(colour = "#1380A1", size = 1) +
    geom_hline(yintercept = 0, size = 8, colour="#333333") +
    bbc_style() +
    labs(title="United States, Personal Consumption Expenditures Price Index",
         subtitle = "Quarterly data: 1959 Q2 - 2019 Q3")
  
  
  # Lays out ts_graph by day with a colored line for each year
#  plot <- ggplot() + 
#    ggtitle(paste("Historical Quarterly Unemployment")) +
#    geom_point(data = ts_graph, aes(x = Quarter, y = Value)) 
  
  finalise_plot(plot_name = line,
                source = "Source: Bureau of Economic Analysis ",
                save_filepath = "./output/quarterly_pce.png",
                width_pixels = 1250,
                height_pixels = 450,
                logo_image_path = "./branding/logo.png")

#  ggsave(filename=paste0("./output/historical-quarterly-unemployment-graph", 
#                         run_date, ".pdf"), plot=ts_plot)

}