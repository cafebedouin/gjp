from datetime import date 
from urllib.request import urlopen 
from bs4 import BeautifulSoup
import re
import requests
import pandas as pd 

# specify url to scrape: %20eq%2012%20and%20year(NEW_DATE)%20eq%202019
# change %2012%20 to month,i.e, 12, surrounded by a space %20
# and last 4 digits are year
# quote_page = 'http://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData?$filter=month(NEW_DATE)%20eq%2012%20and%20year(NEW_DATE)%20eq%202019'  
quote_page = 'data.xml'

# get the date
date = date.today()

# convert to 2020-01-01 format
date = date.strftime("%Y-%m-%d")

# query the url and return the html to the variable 'page'
page = open(quote_page)    # urlopen(quote_page)

#parse the html using beautiful soup and store in variable 'soup'
soup = BeautifulSoup(page, 'html5lib')
name_list = []

# name_store = soup.find('h1', attrs={'div' : 'name'})   
# data_name = name_store.text.strip()  
# price_store = soup.find('div', attrs={'class': 'price'})
# price = price_store.text


for x in soup.find_all('m:properties'):
    list_small = list()
    for y in x:
        date = x.find('d:new_date')
        value = x.find('d:bc_30year')
#    date = date.text.strip()
#    value = value.text.strip()
    print(date, value)

        
#    df = pd.DataFrame(list_small, columns=['time', name])   
#    prev_df = pd.DataFrame.merge(prev_df, df, how='right', on='time')        
#print(prev_df)                                              
#prev_df['time']=pd.to_datetime(prev_df['time'])        
#prev_df.set_index(prev_df['time'],  inplace=True) 
#del prev_df['time'] 

# prev_df.to_csv('/home/scott/Documents/programs/R/forecasting/data/ytest.csv')

