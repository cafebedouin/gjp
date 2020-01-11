# battle_arima.R
#################################################
# Description: Once you download flumart csv, 
# script parses cvs file and provides a graph 
# with selectable yearly trend lines for 
# comparison, also includes analysis options at 
# bottom for predicting a particular day or 
# searching by cases.

#################################################
#PRELIMINARIES

#Clear memory
rm(list=ls())
gc()

#Define basepath and set working directory:
basepath = "~/Documents/programs/R/forecasting"
setwd(basepath)

#Preventing scientific notation in graphs
options(scipen=999)

#################################################
#Libraries 
#If library X is not installed, you can install 
# it with this command: install.packages('X')
library(data.table)
library(lubridate)
library(stringr)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)

#################################################
# Import & Parse
# Download relevant flu data to your computer and point to it.
flumart <- read.csv("~/Documents/hfc/flu/chile.csv", skip=3, header=TRUE)

# Drop all the columns but the ones of interest
flumart <- flumart[ -c(2,3,6:19,21) ]

# Assign column names to something more reasonable
colnames(flumart) <- c("Country", "Year", "Week", "Confirmed_Flu", "Prevalance")  

#Assign the country variable from first column, second row
country <- flumart[c(1),c(1)]

# Incomplete years mess up correlation matrix
flu_table <- filter(flumart, Year >= 2000)

# Drop the non-numerical columns
flu_table <- flu_table[,-c(1,5)]

# Reshape the table into grid
flu_table <- reshape(flu_table, direction="wide", idvar="Week", timevar="Year")

# Fix column names after reshaping
names(flu_table) <- gsub("Confirmed_Flu.", "", names(flu_table))

# Put into matrix for correlations
flu_table <- as.matrix(flu_table[,-c(1,5)])

#################################################
# Correlate & Plot
flu_rcorr <- rcorr(flu_table)
flu_coeff <- flu_rcorr$r
flu_p <- flu_rcorr$P
corrplot(flu_coeff, method="pie", type="lower")

#################################################
# Prediction using ARIMA

flu_data <- flumart # Importing initial flu data
flu_data <- filter(flu_data, Week <= 52)
flu_data <- flu_data[, -c(1:3,5)] # Remove Year & Week
flu_ts <- ts(flu_data, start = 1, frequency=52) # ARIMA needs time series
flu_data <- as.vector(flu_data)
flu_fit <- auto.arima(flu_ts, D=1) 
flu_pred <- forecast(flu_fit, h=52, robust=TRUE)
flu_plot <- as.data.frame(Predicted_Mean <- as.numeric(flu_pred$mean),
                          Upper <- as.numeric(flu_pred$upper),
                          Lower <- as.numeric(flu_pred$lower))
flu_plot$Week <- as.numeric(1:nrow(flu_plot))

ggplot() + 
  ggtitle("Predicted Flu Incidence") +
  geom_line(data = flu_plot, aes(x = Week, y = Predicted_Mean)) + 
  scale_x_continuous() + scale_y_continuous()

#################################################
# Graph

# Creating a temp variable for graph
flu_graph <- flumart

# Uncomment next two lines to filtering results to weeks of interest, then
# comment out 7 year filter below 
# flu_graph <- filter(flu_graph, Week == 22 | Week == 23 | Week == 24 | Week == 25) 
flu_graph <- filter(flu_graph, Year > 2010)

# Filtering results for 7 year comparison, which is busy but readable 
# flu_graph <- filter(flu_graph, Year == 2013 | Year == 2014 | Year == 2015 |
#                      Year == 2016 | Year == 2017 | Year == 2018 | Year == 2019)

# These variables need to be numerical
flu_graph$Week <- as.numeric(flu_graph$Week)
flu_graph$Confirmed_Flu <- as.numeric(flu_graph$Confirmed_Flu)

# The variable used to color and split the data should be a factor so lines are properly drawn
flu_graph$Year <- factor(flu_graph$Year)

# Lays out sea_graph by day with a colored line for each year
ggplot() + 
  ggtitle(paste("Confirmed Flu in", country)) +
  geom_line(data = flu_graph, aes(x = Week, y = Confirmed_Flu, color = Year)) + 
  geom_line(data = flu_plot, aes(x = Week, y = Predicted_Mean, color="Forecast"))
  scale_x_continuous()

# Separate scatterplot for people without RStudio
pdf("hfc_flu.pdf")
plot(x=flu_graph$Day, y=flu_graph$Sea_Ice, type="p", main=paste("Confirmed Flu in", country), 
     xlab="Date", ylab="Confirmed Flu", ylim=c(0, 5000), xlim=c(1, 54))
dev.off()

summary(flu_graph)
sd(flu_graph$Confirmed_Flu)

#################################################
# Scratch
# flu_range_by_week_compare <- filter(flu_graph, Confirmed_Flu < 300)
# flu_range_by_week_compare <- filter(flu_graph, Confirmed_Flu > 100)
#quantile(flu_graph$Confirmed_Flu, c(0, .05, .1, .15, .2, .25,
#                                    .3, .35, .4, .45, .5, .55, .6,
#                                    .65, .7, .75, .8, .85, .9, .95, 1))
