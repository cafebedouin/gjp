# Clear memory
rm(list=ls())
gc()

#################################################
# Set Variables in Function
co2v3 <- function(df_file="./output/co2-table-change.csv") {
  
  #################################################
  #PRELIMINARIES
  
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
  df <- read.csv(df_file, header=TRUE)
  
  colnames(df) <- c("Day", "Year", "Value")  
  
  #################################################
  # Graph
  
  # Filtering results for 5 year comparison, against 5 closest correlated years 
#  df <- filter(df, Year == 2020 | Year == 2019 |
#                   Year == 2018  | Year == 2017 | 
#                   Year == 2016  | Year == 2012 | 
#                   Year == 2011 | Year == 2015)

  df$Day <- as.numeric(df$Day)
  df$Value <- as.numeric(df$Value)
  df$Year <- factor(df$Year)
  
  # Lays out sea_graph by day with a colored line for each year
  df_compare <- ggplot() + 
    ggtitle(paste("Rate of CO2 growth by year")) +
    geom_line(data = df, aes(x = Day, y = Value, color = Year)) + 
    #    geom_line(data = df_plot, aes(x = Week, y = Predicted_Mean, color="Forecast"))
    scale_x_continuous()
  
  # Print flu_compare to screen
  df_compare
  
  # Print df to PDF
  ggsave(filename=paste0("./output/co2-graph.pdf"), plot=df_compare)
  
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