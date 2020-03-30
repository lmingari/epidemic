#!/usr/bin/env python

import pandas as pd
import matplotlib.pyplot as plt
from os.path import join

threshold = 100
fig, (ax, ax2) = plt.subplots(2,1)

url="https://raw.githubusercontent.com/datasets/covid-19/master/data/key-countries-pivoted.csv"
data = pd.read_csv(url,parse_dates=[0])
print(data)

items = ['US']
for item in items: 
    x = data[item].loc[data[item]>threshold]
    x.index -= x.index[0]
    threshold_new = x.iloc[0]
    #
    for output in ['free.dat','qmin.dat','control.dat']:
        df = pd.read_csv(join(item,output),
                         delim_whitespace = True, 
                         names = ["time","i","r"])
        df['dr'] = df['r'].diff()
        t0=(df['r']-threshold_new).abs().argsort()[0]
        df['time'] -= t0
        df.plot(x='time',y='r',  logy=True, label=output, ax=ax)
        df.plot(x='time',y='dr', logy=True, label=output, ax=ax2)
    x.plot(style=".", logy=True, label=item, ax=ax )
    x.diff().plot(style=".", logy=True, label=item, ax=ax2 )

l1 = ax.legend()
l2 = ax2.legend()

plt.show()
