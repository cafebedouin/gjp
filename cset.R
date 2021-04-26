source("./functions/standist.R")
standist(filename="cset-106",
         forecast_periods=2,
         outlier=11:13, 
         bins=c(200000000,350000000,500000000,650000000,Inf))

source("./functions/standist.R")
standist(filename="cset-107",
         forecast_periods=2,
         outlier=11:13, 
         bins=c(30000000,70000000,110000000,150000000,Inf))

source("./functions/standist.R")
standist(filename="cset-123",
         forecast_periods=2,
         outlier=22:26, 
         bins=c(0.055,0.07,0.095,0.12,Inf))

source("./functions/standist.R")
standist(filename="cset-128",
         forecast_periods=2,
         outlier=NULL,
         bins=c(25000000000,35000000000,45000000000,55000000000,Inf))

source("./functions/standist.R")
standist(filename="cset-129",
         forecast_periods=2,
         outlier=NULL,
         bins=c(320000000000,350000000000,380000000000,410000000000,Inf))        

source("./functions/standist.R")
standist(filename="cset-130",
         forecast_periods=2,
         outlier=NULL,
         bins=c(8500000000,10000000000,11500000000,13000000000,Inf))

source("./functions/standist.R")
standist(filename="cset-131",
         forecast_periods=2,
         outlier=NULL,
         bins=c(5000000000,6000000000,7000000000,8000000000,Inf))

