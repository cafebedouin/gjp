# threshold.R 
#################################################
# Description:This is a functional program that 
# takes closing dates, calls a data function
# recursively puts them through a data extraction, then
# a Monte Carlo or simple historical probability
# script and then puts the probability for each
# date into a vector and sums it to assess the 
# chance of hitting a threshold over a given period.

source("fred.R")
closing_date <- seq(as.Date("2021-06-01", "%Y-%m-%d"), 
                    by = month, 
                    length.out = 5)

View(closing_date)

total <-0

# This calls the function using different dates and returns a df
# with two columns: bins and probs. It then puts the probabilities
# in calc and when finished going through all dates, sums the result.

for (i in 1:length(closing_date)) {
  threshold <- fred(code="TRUCKD11",
                    begin_date="2000-01-01", # For analysis, not question
                    closing_date[i],
                    # Threshold value 
                    bins=c(120, Inf),
                    probability_type="monte")
  total[i] <- threshold[2,2]
}

sum(total)
