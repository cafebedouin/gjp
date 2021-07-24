# threshold.R 
#################################################
# Description:This is a functional program that 
# takes closing dates calls a data function
# recursively puts them through a Monte 
# Carlo or simple historical probability
# script and then puts the probability for each
# date into a vector and sums it to assess the 
# chance of hitting a threshold over a given period.

source("fred.R")
closing_date <- as.Date(c("2021-08-01",
                          "2021-09-01", 
                          "2021-10-01",
                          "2021-11-01", 
                          "2021-12-01", 
                          "2022-01-01"), 
                        "%Y-%m-%d")

total <-0

# Alternatively call a csv file fir closing dates
# closing_date <- read.csv(paste0("~/Documents/programming/R/data/threshold.csv), 
#               na.strings = "", fileEncoding = "UTF-8-BOM",
#               skip=0, header=TRUE)

# This calls the function using different dates and returns a df
# with two columns: bins and probs. It then puts the probabilities
# in calc and when finished going through all dates, sums the result.
for (i in 1:length(closing_date)) {
  threshold <- csv(#code="TRUCKD11",
                   begin_date="2000-01-01", # For analysis, not question
                   closing_date[i],
                   freq="monthly",
                   # Threshold value 
                   bins=c(120, Inf),
                   probability_type="monte")
  total[i] <- threshold[2,2]
}

sum(total)
