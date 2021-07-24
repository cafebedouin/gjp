#!/usr/bin/env python
import pandas as pd
import random
import datetime

class Simulation:
    def __init__(self,symbol,expiration_date,lower_bound,upper_bound,current_price=None,current_price_weight=.2):

        get_price =  lambda ticker: float(pd.read_html('https://finance.yahoo.com/quote/' + ticker + '/history')[0]["Close*"].iloc[0])

        self.lower_bound = lower_bound
        self.upper_bound = upper_bound

        split_date = [int(x) for x in expiration_date.split('-')]
        expiration = datetime.date(*split_date)
        self.remaining_days = (expiration - datetime.date.today()).days
        self.current_price = current_price
        if current_price is None:
            self.current_price = get_price(symbol)
        print("CURRENT PRICE:",self.current_price)
        self.current_price_weight = current_price_weight
        self.simulated_price_weight = 1 - current_price_weight
        df = pd.read_csv(symbol + ".csv")
        df["Previous"] = df.Close.shift()
        df.dropna(inplace=True)
        self.shifts = set((df.Close/df.Previous).apply(lambda x: x - 1)) # Make a set of historical price shifts

    def run_trials(self,trials=10000,sudden_condition=False):
        sample = lambda data: random.sample(data,1)[0] # Pull a random sample from a set
        random_flip = lambda: [-1,1,1][random.randrange(3)] # 1 / 3 chance of flipping the sign
        low = 0 
        high = 0
        for j in range(trials):
            price = self.current_price
            for i in range(self.remaining_days):
                move = sample(self.shifts) * (self.current_price_weight * self.current_price + self.simulated_price_weight * price) * random_flip()
                price = price  + move
                if sudden_condition:
                    if price < self.lower_bound:
                        low += 1
                        break
                    if price > self.upper_bound:
                        high += 1
                        break
            if not sudden_condition:
                if price < self.lower_bound:
                    low += 1
                if price > self.upper_bound:
                    high += 1

        inputs = [x * 100 / trials for x in [low,high]]
        print("LOW:{}%,HIGH:{}%".format(*inputs))


if __name__ == '__main__':
    s = Simulation('BTC-USD','2021-07-02',25000,100000)
    s.run_trials()
