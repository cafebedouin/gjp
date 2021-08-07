# threshold.R 
#################################################
# Description:This is a functional program that 
# takes closing dates, calls a data function
# recursively puts them through a data extraction, then
# a Monte Carlo or simple historical probability
# script and then puts the probability for each
# date into a vector and sums it to assess the 
# chance of hitting a threshold over a given period.

source("csv-oil.R")
closing_date <- seq(as.Date("2021-08-07", "%Y-%m-%d"), 
                    by = "week", 
                    length.out = 21)

# View(closing_date)
 
probs <-0

# This calls the function using different dates and returns a df
# with two columns: bins and probs. It then puts the probabilities
# in calc and when finished going through all dates, sums the result.

for (i in 1:length(closing_date)) {
  threshold <- csv(begin_date = "1999-12-31", # For analysis, not question
                   closing_date = closing_date[i],
                   # Threshold value 
                   bins=c(12000, Inf),
                   probability_type="monte")
  probs[i] <- threshold[2,2]
}
probs

for (i in 1:length(probs)) {
  probs[i] <- probs[i] / i  
}

probs <- 1 - probs
probs
total <- probs[1]

for (i in 2:length(probs)) {
  total <- probs[i] * total 
}
total <- 1 - total
total

