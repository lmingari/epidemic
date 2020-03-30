#!/usr/bin/env python

import pandas as pd
import matplotlib.pyplot as plt

threshold = 100
fig, (ax, ax2) = plt.subplots(2,1)

data1 = pd.read_csv("output_free.dat", delim_whitespace=True, names=["time","i","r"], index_col=0)
t0 = data1.loc[data1["r"]>threshold].index[0]
data1.index -= t0
data1.plot(y='r', logy=True, label="model free", ax=ax)
data1.diff().plot(y='r', logy=True, label="model free", ax=ax2)

data2 = pd.read_csv("output_qmin.dat", delim_whitespace=True, names=["time","i","r"], index_col=0)
t0 = data2.loc[data2["r"]>threshold].index[0]
data2.index -= t0
data2.plot(y='r', logy=True, label="model qmin", ax=ax)
data2.diff().plot(y='r', logy=True, label="model qmin", ax=ax2)

data3 = pd.read_csv("output_control.dat", delim_whitespace=True, names=["time","i","r"], index_col=0)
t0 = data3.loc[data3["r"]>threshold].index[0]
data3.index -= t0
data3.plot(y='r', logy=True, label="model control", ax=ax)
data3.diff().plot(y='r', logy=True, label="model control", ax=ax2)

url="https://raw.githubusercontent.com/datasets/covid-19/master/data/key-countries-pivoted.csv"
data = pd.read_csv(url,parse_dates=[0])

print(data)
items = ['Spain']
for item in items: 
    t0 = data[item].loc[data[item]>threshold].index[0]
    x = data[item]
#    x = data[item].loc[data[item]>threshold]
    x.index = x.index-t0
    x.plot( logy=True, label=item, ax=ax )
#    x2 = data[item]
#    print(x2)
    x.diff().plot( logy=True, label=item, ax=ax2 )
plt.legend()
plt.show()
