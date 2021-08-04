# sortbins.R 
#################################################
# Description: Sorts probability bins

sortbins <- function(bins,
                     prob_calc) {
  
  library(stats) 
  sort(bins)
  bins <-data.frame(bins)
  # View(bins)
  
  # Sort probabilities into bins
  for (i in 1:length(bins$bins)) {
    if (i == 1) {
      # Checks to see probabilities below lowest value
      bins$probs[i] <- round(sum(prob_calc<bins$bins[i])/length(prob_calc), digits = 3)
    } else if ((i %% 2) == 0) {
      bins$probs[i] <- round(sum(prob_calc>=bins$bins[i-1] & prob_calc<=bins$bins[i])/length(prob_calc), digits = 3)
    } else if ((i %% 2) != 0) {
      bins$probs[i] <- round(sum(prob_calc>bins$bins[i-1] & prob_calc<bins$bins[i])/length(prob_calc), digits = 3)
    }
  }
  return(bins)
}