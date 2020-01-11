# correlation_pie.R 
#################################################
# Description: Takes a matrix of years, numerical
# day of year and
# and turns it into year, day, value and then
# puts it into a matrix, exported to csv.

#################################################
# Correlation Matrix
correlation_pie <- function(day_one=1, last_day=336) {
df_rcorr <- rcorr(as.matrix(df)
df_coeff <- df_rcorr$r

pdf(paste0("./output/correlation-",  todays_date, ".pdf"))
corrplot(df_coeff, method="pie", type="lower")
dev.off()