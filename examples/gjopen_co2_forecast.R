Co2 <- read.table("ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_trend_gl.txt", comment.char="#", header=FALSE)

Co2_cycle_ts <- ts(Co2$V4, start=c(2010, 1), frequency=365)

Seas <- cycle(Co2_cycle_ts)
Time <- time(Co2_cycle_ts)
Co2.lm <- lm(Co2_cycle_ts ~ 0 + Time + factor(Seas))

# Forecast
new.t <- seq(2020, len=12, by=1/12)
alpha <- coef(Co2.lm)[1]
beta <- rep(coef(Co2.lm)[2:13], 2)

new.dat <- data.frame(Time = new.t, Seas=seq(1:12))
print("----")
print(predict(Co2.lm, new.dat, interval="prediction", level=0.995)[12,])
print(predict(Co2.lm, new.dat, interval="prediction", level=0.99)[12,])
print(predict(Co2.lm, new.dat, interval="prediction", level=0.975)[12,])
print(predict(Co2.lm, new.dat, interval="prediction", level=0.95)[12,])
print(predict(Co2.lm, new.dat, interval="prediction", level=0.90)[12,])
print(predict(Co2.lm, new.dat, interval="prediction", level=0.85)[12,])
print(predict(Co2.lm, new.dat, interval="prediction", level=0.80)[12,])
```

Gives output:

```
fit lwr upr
414.5935 413.3501 415.8368
fit lwr upr
414.5935 413.4527 415.7343
fit lwr upr
414.5935 413.6009 415.5861
fit lwr upr
414.5935 413.7256 415.4613
fit lwr upr
414.5935 413.8652 415.3218
fit lwr upr
414.5935 413.9561 415.2308
fit lwr upr
414.5935 414.0261 415.1608
```

The lower limit is above 413 in all cases, so set <412 as 0%. 414 ppm only shows up when level=0.8, so <414=10%. Next, 416 never shows up, so set this to 0%. Therefore, [414, 416] gets 90%, which roughly matches the levels given.

This seems odd, so I'll sanity check with @Hammer-Time's data [2]:

```
# Long term linear model
Decs <- c(401.36, 404.23, 406.47, 408.93, 411.44)
Yrs <- seq(1:length(Decs))
Decs_trend <- data.frame(Dec=Decs, Yr=Yrs)
Long.lm <- lm(Dec ~ Yr, Decs_trend)
new.dat <- data.frame(Yr=c(length(Decs)+1))
# Forecasts
print("----")
print(predict(Long.lm, new.dat, level=0.995, interval="prediction"))
print(predict(Long.lm, new.dat, level=0.99, interval="prediction"))
print(predict(Long.lm, new.dat, level=0.975, interval="prediction"))
print(predict(Long.lm, new.dat, level=0.95, interval="prediction"))
print(predict(Long.lm, new.dat, level=0.90, interval="prediction"))
```

This has output:

```
fit lwr upr
1 413.944 412.1917 415.6963
fit lwr upr
1 413.944 412.5708 415.3172
fit lwr upr
1 413.944 412.9621 414.9259
fit lwr upr
1 413.944 413.1958 414.6922
fit lwr upr
1 413.944 413.3907 414.4973
```