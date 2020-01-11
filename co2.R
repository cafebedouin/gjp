# co2_doy.R 
#################################################
# License: 
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

#################################################
# Description: Pulls MASIE ice data, limits by sea, and runs a 
# correlation matrix to determine which five years have trend lines 
# that are closest to current, and presents recent data in a graph. 
# Best use is in generating trend lines and being able to visually 
# compare current year with previous years.
#
# Note: The prediction algorithm, ARIMA, is commented out because it
# is of questionable utility and makes this script run much slower.
# I left it in as a bad example for people that want to try machine 
# prediction.

rm(list=ls()) # Clear memory
gc()

# Set Variables in Function
# "Northern_Hemisphere", "Beaufort_Sea", "Chukchi_Sea", "East_Siberian_Sea", 
# "Laptev_Sea", "Kara_Sea", "Barents_Sea", "Greenland_Sea",
# "Baffin_Bay_Gulf_St._Lawrence", "Canadian_Archipelago", "Hudson_Bay", 
# "Central_Arctic", "Bering_Sea", "Baltic_Sea", "Sea_of_Okhotsk", 
# "Yellow_Sea", "Cook_Inlet"

# Replace defaults in function to desired, or call the function from console
defun_co2 <- function(day_one=1, last_day=336) {
  days_start <- day_one
  days_end <- last_day
  co2_date <- Sys.Date()

  #################################################
  # Preliminaries

  # Define basepath and set working directory:
  basepath = "~/Documents/R/forecasting"
  setwd(basepath)

  # Preventing scientific notation in graphs
  options(scipen=999)

  #################################################
  # Load libraries. If library X is not installed
  # you can install it with this command at the R prompt:
  # install.packages('X') 
  library(data.table)
  library(lubridate)
  library(stringr)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(reshape2)
  library(corrplot)
  library(hydroGOF)
  library(Hmisc)
  library(forecast)
  library(tseries)

  #################################################
  # Import, organize and output csv data
  #################################################
  # Import & Parse
  # Point to time series data file and import it.
  co2_data <- read.fwf("~/Downloads/co2_trend_gl.txt", 
                          widths = c(6,6,6,9,9),
                          header=FALSE)
  
  # Combine year, month, and day into a date
  co2_data$V1 <- paste0(co2_data$V1, "-", 
                           co2_data$V2, "-", 
                           co2_data$V3)
  
  # Drop the extra columns
  co2_data <- co2_data[-c(3,5)]

  # Naming columns
  colnames(co2_data) <- c("Year", "Day", "Value")
  
  co2_data$Year <- as.Date(co2_data$Year)
  co2_data$Day <- as.Date(co2_data$Year) 
  
  # Puts into date decreasing order
  # co2_data <- co2_data[rev(order(as.Date(co2_data$Year), na.last = NA)),]
  
  co2_data$Day <- format(co2_data$Day, "%j")
  co2_data$Year <- format(co2_data$Year, "%Y")
  
  # Reshape the three columns into a table, fix names, and export csv file
  co2_table <- reshape(co2_data, direction="wide", idvar="Day", timevar="Year")
  names(co2_table) <- gsub("Value.", "", names(co2_table))
  
  # Creating a cvs file of changed data
  write.csv(co2_table, file=paste0("./output/co2-table-", co2_date, ".csv")) 
  

  #################################################
  # Correlation Matrix
  co2_rcorr <- rcorr(as.matrix(co2_table))
  co2_coeff <- co2_rcorr$r

#  pdf(paste0("./output/co2-correlation-", co2_date, ".pdf"))
#  corrplot(co2_coeff, method="pie", type="lower")
#  dev.off()
  
  # Takes current year, sorts the matches, take the year column names,
  # and then assigns them values for the graph
  co2_matches <- co2_coeff[,ncol(co2_coeff)]
  co2_matches <- sort(co2_matches, decreasing = TRUE)
  co2_matches <- names(co2_matches)
  current_year <- as.numeric(co2_matches[1])
  matching_year1 <- as.numeric(co2_matches[2])
  matching_year2 <- as.numeric(co2_matches[3])
  matching_year3 <- as.numeric(co2_matches[4])
  matching_year4 <- as.numeric(co2_matches[5])
  matching_year5 <- as.numeric(co2_matches[6])
  
  #################################################
  # Historic Trends Graph
  co2_graph <- co2_data

  # Filtering results for 5 year comparison, against 5 closest correlated years 
#  co2_graph <- filter(co2_graph, Year == current_year | Year == matching_year1 |
#                      Year == matching_year2 | Year == matching_year3 | 
#                      Year ==matching_year4 | Year == matching_year5)
  
  # Filtering results for 5 year comparison, against 5 closest correlated years 
  co2_graph <- filter(co2_graph, Year == 2019 | Year == 2018 |
                        Year == 2017 | Year == 2016 | 
                        Year == 2015 | Year == 2020)

  # These variables need to be numerical
  co2_graph$Day <- as.numeric(co2_graph$Day)
  co2_graph$Value <- as.numeric(co2_graph$Value)

  # Filtering results into period of interest
  co2_graph <- filter(co2_graph, Day <= days_end)  
  co2_graph <- filter(co2_graph, Day >= days_start)

  # The variable used to color and split the data should be a factor so lines are properly drawn
  co2_graph$Year <- factor(co2_graph$Year)

  # Lays out sea_graph by day with a colored line for each year
  co2_plot <- ggplot() + 
    ggtitle(paste("CO2 levels")) +
    geom_line(data = co2_graph, aes(x = Day, y = Value, color = Year)) + 
    # Uncomment this if you have a sensible forecast algorithm.
    # geom_line(data = sea_plot, aes(x = Day, y = Predicted_Mean, color="Forecast")) + 
    scale_x_continuous()

  co2_plot
  ggsave(filename=paste0("./output/co2-plot-", co2_date, ".pdf"), plot=co2_plot)

  summary(co2_graph)
  sd(co2_graph$Values)

  return(co2_graph[nrow(co2_graph), ncol(co2_graph)])
}

#################################################
# Scratch Code
#sea_area_day_compare <- filter(sea_area, Day == 100)
#quantile(sea_graph$Sea_Ice, c(0, .05, .1, .15, .2, .25,
#                              .3, .35, .4, .45, .5, .55, .6,
#                              .65, .7, .75, .8, .85, .9, .95, 1))