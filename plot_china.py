#!/usr/bin/env python

import pandas as pd
import matplotlib.pyplot as plt
from os.path import join

threshold = 100
fig, (ax, ax2) = plt.subplots(2,1)

df=pd.read_csv("total-and-daily-cases-covid-19.csv",parse_dates=["Date"])
df.rename(columns={ df.columns[3]: "r",  df.columns[4]: "dr" }, inplace = True)

data = df.loc[df["Code"]=="CHN"]
#data = data.loc[data.iloc[:,3]>threshold]
t0=data.Date.loc[data.r>threshold].iloc[0]
data.loc[:,"days"] = (data["Date"]-t0).dt.days
threshold_new = data.r.loc[data.r>100].iloc[0]

item="China"
for output in ['free.dat','qmin.dat','control.dat']:
    df = pd.read_csv(join(item,output),
                     delim_whitespace = True,
                     names = ["time","i","r"])
    df['dr'] = df['r'].diff()
    t0=(df['r']-threshold_new).abs().argsort()[0]
    df['time'] -= t0
    df.plot(x='time',y='r',  logy=True, label=output, ax=ax)
    df.plot(x='time',y='dr', logy=True, label=output, ax=ax2)

data.plot(style=".", x="days", y=3, logy=True, ax=ax)
data.plot(style=".", x="days", y=4, logy=True, ax=ax2)


plt.show()
