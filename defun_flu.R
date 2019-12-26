# defun_flu.R
#################################################
# Prep: Go to the WHO Flumart website:
# http://apps.who.int/flumart/Default?ReportNo=12
# Select Year 2000 Week 1 to current year, week 52.
# Save file to the data directory. Change weeks below. 

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

#################################################
# Set Variables in Function
flu <- function(flu_file="./data/FluNetInteractiveReport.csv",
                week_start=1, week_end=52) {

  #################################################
  #PRELIMINARIES

  #Define basepath and set working directory:
  basepath = "~/Documents/programs/R/forecasting"
  setwd(basepath)

  #Preventing scientific notation in graphs
  options(scipen=999)

  #################################################
  #Libraries 
  # If library X is not installed, you can install 
  # it with this command: install.packages('X')
  library(plyr)
  library(tidyr)
  library(data.table)
  library(lubridate)
  library(stringr)
  library(ggplot2)
  library(dplyr)
  library(reshape2)
  library(corrplot)
  library(hydroGOF)
  library(Hmisc)
  library(forecast)
  library(tseries)

  #################################################
  # Import & Parse
  # Point to downloaded flu data file, variable is above.
  flumart <- read.csv(flu_file, skip=3, header=TRUE) 

  # Drop all the columns but the ones of interest
  flumart <- flumart[ -c(2,3,6:19,21) ]

  # Assign column names to something more reasonable
  colnames(flumart) <- c("Country", "Year", "Week", "Confirmed_Flu", "Prevalance")  

  # Assign the country variable from first column, second row
  country <- flumart[c(1),c(1)]

  # Incomplete years mess up correlation matrix
  flu_table <- filter(flumart, Year >= 2000)
  View(flu_table)

  # Drop the non-numerical columns
  flu_table <- flu_table[,-c(1,5)]

  # Reshape the table into grid
  flu_table <- reshape(flu_table, direction="wide", idvar="Week", timevar="Year")
  View(flu_table)

  # Fix column names after reshaping
  names(flu_table) <- gsub("Confirmed_Flu.", "", names(flu_table))
  
  # Put into matrix for correlations
  flu_table <- as.matrix(flu_table[,-c(1,5)])

  #################################################
  # Correlate & Plot
  flu_rcorr <- rcorr(flu_table)
  flu_coeff <- flu_rcorr$r
  flu_p <- flu_rcorr$P

  flu_matches <- flu_coeff[,ncol(flu_coeff)]
  flu_matches <- sort(flu_matches, decreasing = TRUE)
  flu_matches <- names(flu_matches)
  current_year <- as.numeric(flu_matches[1])
  matching_year1 <- as.numeric(flu_matches[2])
  matching_year2 <- as.numeric(flu_matches[3])
  matching_year3 <- as.numeric(flu_matches[4])
  matching_year4 <- as.numeric(flu_matches[5])
  matching_year5 <- as.numeric(flu_matches[6])

  #################################################
  # Graph

  # Creating a temp variable for graph
  flu_graph <- flumart

  # Filtering results for 5 year comparison, against 5 closest correlated years 
  flu_graph <- filter(flu_graph, Year == current_year | Year == matching_year1 |
                      Year == matching_year2 | Year == matching_year3 | 
                      Year ==matching_year4 | Year == matching_year5)

  # These variables need to be numerical
  flu_graph$Week <- as.numeric(flu_graph$Week)
  flu_graph$Confirmed_Flu <- as.numeric(flu_graph$Confirmed_Flu)

  # Limit to weeks of interest
  flu_graph <- filter(flu_graph, Week >= week_start)
  flu_graph <- filter(flu_graph, Week <= week_end)  

  # The variable used to color and split the data should be a factor so lines are properly drawn
  flu_graph$Year <- factor(flu_graph$Year)

  # Lays out sea_graph by day with a colored line for each year
  flu_compare <- ggplot() + 
    ggtitle(paste("Confirmed Flu in", country)) +
    geom_line(data = flu_graph, aes(x = Week, y = Confirmed_Flu, color = Year)) + 
#    geom_line(data = flu_plot, aes(x = Week, y = Predicted_Mean, color="Forecast"))
    scale_x_continuous()

  flu_week <- flu_graph[nrow(flu_graph),3]
  flu_year <- flu_graph[nrow(flu_graph),2]
  summary(flu_graph)

  ###############################################
  # Printing
  # Creating a cvs file of changed data
  write.csv(flu_table, file=paste0("./output/flu-in-", country, "-table-",
                                   flu_year, "-week-", flu_week, ".csv")) 
  
  # Print flu_compare to screen
  flu_compare
  
  # Print flu
#  ggsave(filename=paste0("./output/flu-in-", country, 
#                         "-prediction-", flu_year, "-week-", flu_week, ".pdf"), plot=flu_prediction)
  
  # Print flu_plot to PDF
  ggsave(filename=paste0("./output/flu-in-", country, 
                         "-compare-", flu_year, "-week-", flu_week, ".pdf"), plot=flu_compare)
  
  # Print correlation matrix
  pdf(paste0("./output/flu-in-", country, "-correlation-", 
             flu_year, "-week-", flu_week, ".pdf"))
  corrplot(flu_coeff, method="pie", type="lower")
  dev.off()
    
  return(flu_graph[nrow(flu_graph),])
}

#################################################
# Scratch
# Separate scatterplot for people without RStudio
# pdf("./output/scatterplot_flu.pdf")
# plot(x=flu_graph$Day, y=flu_graph$Sea_Ice, type="p", main=paste("Confirmed Flu in", country), 
#     xlab="Date", ylab="Confirmed Flu", ylim=c(0, 5000), xlim=c(1, 54))
# dev.off()

# flu_range_by_week_compare <- filter(flu_graph, Confirmed_Flu < 300)
# flu_range_by_week_compare <- filter(flu_graph, Confirmed_Flu > 100)
# quantile(flu_graph$Confirmed_Flu, c(0, .05, .1, .15, .2, .25,
#                                    .3, .35, .4, .45, .5, .55, .6,
#                                    .65, .7, .75, .8, .85, .9, .95, 1))