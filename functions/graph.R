# graph.R 
#################################################
# Description: Generates a graph from time series
# using BBC style: https://bbc.github.io/rcookbook/
# See url for initial set-up.

rm(list=ls()) # Clear memory
gc()

# Replace defaults in function to desired, or call the function from console
graph <- function(df,
                  title="",
                  subtitle="",
                  info_source="",
                  file_name="default",
                  graph_width=1250,
                  graph_height=450) {
  
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
  todays_date <- Sys.Date()

  # Adding column names
  colnames(df) <- c("Date", "Value")
  
  #Make plot
  line <- ggplot(df, aes(x = Date, y = Value)) +
    geom_line(colour = "#1380A1", size = 1) +
    geom_hline(yintercept = 0, size = 8, colour="#333333") +
    #  geom_hline(yintercept = hline, size = 8, colour="#333333") +
    bbc_style() +
    labs(title= title,
         subtitle = subtitle) +
    geom_vline(xintercept=todays_date, linetype=4, colour="black")

# Lays out ts_graph by day with a colored line for each year
# plot <- ggplot() + 
#    ggtitle(paste("Historical Quarterly Unemployment")) +
#    geom_point(data = ts_graph, aes(x = Quarter, y = Value)) 
  
  finalise_plot(plot_name = line,
                source = info_source,
                save_filepath = paste0("./output/", file_name,
                                       "-", todays_date, ".png"),
                width_pixels = graph_width,
                height_pixels = graph_height,
                logo_image_path = "./branding/logo.png")

}