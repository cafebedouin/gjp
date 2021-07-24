data = pd.read_excel("./Data/CO2ppm.xlsx")
data.index = pd.date_range(start = '2010-01-01-00', end = '2020-01-03-00', freq= 'D')
data = data['cycle']
# Get first day of every month
data = data.resample('MS').apply(lambda ser: ser.iloc[-1,])

# ~~~
# Fit model
# ~~~

mod = sm.tsa.statespace.SARIMAX(data,
order=(1, 1, 1),
seasonal_order=(0, 1, 1, 12),
enforce_stationarity=False,
enforce_invertibility=False)
results = mod.fit()

# ~~~
# Predict
# ~~~

pred = results.get_forecast(steps=11)
pred_ci = pred.conf_int(alpha=0.05)
