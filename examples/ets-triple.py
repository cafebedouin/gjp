Python code:

# ~~~
# Holt winters, aka ETS triple
# ~~~

from statsmodels.tsa.holtwinters import ExponentialSmoothing

model = ExponentialSmoothing(data, trend='mul', seasonal='add', damped=False)

model_fit = model.fit()

# ~~~
# Predict
# ~~~

pred_ets = model_fit.forecast(11)

ax = data[-48:].plot(label='observed', figsize=(14, 7))
pred_ets.plot(ax=ax, label='Forecast')
ax.set_xlabel('Date')
ax.set_ylabel('CO2')
plt.legend()
plt.show()
