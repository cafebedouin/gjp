# defun_sea_ice.R 
#################################################
# Description: Pulls MASIE ice data, limits by sea, and runs a 
# correlation matrix to determine which five years have trend lines 
# that are closest to current, and presents recent data in a graph. 
# Best use is in generating trend lines and being able to visually 
# compare current year with previous years.

rm(list=ls()) # Clear memory
gc()

# Select one of the following for sea
# "Northern_Hemisphere", "Beaufort_Sea", "Chukchi_Sea", "East_Siberian_Sea", 
# "Laptev_Sea", "Kara_Sea", "Barents_Sea", "Greenland_Sea",
# "Baffin_Bay_Gulf_St._Lawrence", "Canadian_Archipelago", "Hudson_Bay", 
# "Central_Arctic", "Bering_Sea", "Baltic_Sea", "Sea_of_Okhotsk", 
# "Yellow_Sea", "Cook_Inlet"

# Replace defaults in function to desired, or call the function from console
defun_sea_ice <- function(sea="Baltic_Sea", 
                          day_one=1, 
                          last_day=121) {
  days_start <- day_one
  days_end <- last_day
  sea_of_interest <- sea
  sea_date <- Sys.Date()

  #################################################
  # Preliminaries

  # Define basepath and set working directory:
  basepath = "~/Documents/programs/R/forecasting"
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

  # Testing
  # masie <- read.csv("./data/masie_4km_allyears_extent_sqkm.csv", skip=1, header=TRUE)

  # Import csv using current masie data
  masie <- read.csv("ftp://sidads.colorado.edu/DATASETS/NOAA/G02186/masie_4km_allyears_extent_sqkm.csv", skip=1, header=TRUE)
  # masie <- read.csv("./data/masie_4km_allyears_extent_sqkm.csv", skip=1, header=TRUE)

  # Assign column names, gets rid of (X) in column names
  colnames(masie) <- c("yyyyddd", "Northern_Hemisphere", "Beaufort_Sea", "Chukchi_Sea", 
              "East_Siberian_Sea", "Laptev_Sea", "Kara_Sea", "Barents_Sea", "Greenland_Sea",
              "Baffin_Bay_Gulf_St._Lawrence", "Canadian_Archipelago", "Hudson_Bay", 
              "Central_Arctic", "Bering_Sea", "Baltic_Sea", "Sea_of_Okhotsk", "Yellow_Sea", 
              "Cook_Inlet") 

  # Separating day and year
  masie_tmp <- tidyr::extract(masie, yyyyddd, into = c("Year", "Day"), "(.{4})(.{3})", remove=FALSE)

  # Selecting the columns of interest
  sea_select <- c("Year", "Day", sea_of_interest)

  # Pulling column data from masie and putting it in the data frame
  sea_area <- masie_tmp[sea_select] 

  # Adding column names, changing sea name to sea ice
  colnames(sea_area) <- c("Year", "Day", "Sea_Ice")

  # Reshape the three columns into a table, fix names, and export csv file
  sea_table <- reshape(sea_area, direction="wide", idvar="Day", timevar="Year")
  names(sea_table) <- gsub("Sea_Ice.", "", names(sea_table))

  # Creating a cvs file of changed data
  write.csv(sea_table, file=paste0("./output/sea-ice-in-", sea, "-table-", sea_date, ".csv")) 

  #################################################
  # Correlation Matrix
  sea_rcorr <- rcorr(as.matrix(sea_table[, -c(1)]))
  sea_coeff <- sea_rcorr$r

  pdf(paste0("./output/sea-ice-in-", sea, "-correlation-", sea_date, ".pdf"))
  corrplot(sea_coeff, method="pie", type="lower")
  dev.off()
  
  # Takes current year, sorts the matches, take the year column names,
  # and then assigns them values for the graph
  sea_matches <- sea_coeff[,ncol(sea_coeff)]
  sea_matches <- sort(sea_matches, decreasing = TRUE)
  sea_matches <- names(sea_matches)
  current_year <- as.numeric(sea_matches[1])
  matching_year1 <- as.numeric(sea_matches[2])
  matching_year2 <- as.numeric(sea_matches[3])
  matching_year3 <- as.numeric(sea_matches[4])
  matching_year4 <- as.numeric(sea_matches[5])
  matching_year5 <- as.numeric(sea_matches[6])
  
  #################################################
  # Historic Trends Graph
  sea_graph <- sea_area

  # Filtering results for 5 year comparison, against 5 closest correlated years 
  sea_graph <- filter(sea_graph, Year == current_year | Year == matching_year1 |
                      Year == matching_year2 | Year == matching_year3 | 
                      Year ==matching_year4 | Year == matching_year5)

  # These variables need to be numerical
  sea_graph$Day <- as.numeric(sea_graph$Day)
  sea_graph$Sea_Ice <- as.numeric(sea_graph$Sea_Ice)

  # Filtering results into period of interest
  sea_graph <- filter(sea_graph, Day <= days_end)  
  sea_graph <- filter(sea_graph, Day >= days_start)

  # The variable used to color and split the data should be a factor so lines are properly drawn
  sea_graph$Year <- factor(sea_graph$Year)

  # Lays out sea_graph by day with a colored line for each year
  sea_plot <- ggplot() + 
    ggtitle(paste("Incidence of Sea Ice in", sea_of_interest)) +
    geom_line(data = sea_graph, aes(x = Day, y = Sea_Ice, color = Year)) + 
    scale_x_continuous()

  sea_plot
  ggsave(filename=paste0("./output/sea-ice-in-", sea_of_interest, 
                         "-plot-", sea_date, ".pdf"), plot=sea_plot)

  summary(sea_graph)
  sd(sea_graph$Sea_Ice)

  return(sea_graph[nrow(sea_graph), ncol(sea_graph)])
}
