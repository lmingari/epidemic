#!/usr/bin/env python

import pandas as pd
import matplotlib.pyplot as plt
from os.path import join

plt.rcParams.update({'font.size': 8})

threshold = 100
fig, axes = plt.subplots(2,1)

url="https://raw.githubusercontent.com/datasets/covid-19/master/data/key-countries-pivoted.csv"
data = pd.read_csv(url,parse_dates=[0])
print(data)

labels = {'free.dat':    "free evolution",
          'qmin.dat':    "self-protective behavior",
          'control.dat': "isolation",
          }

items = ['Italy']
for item in items: 
    fname = join(item,"plot.png")
    t0 = data.loc[data[item]>=threshold].Date.iloc[0]
    data.loc[:,"days"]   = (data.Date-t0).dt.days
    data.loc[:,"ncases"] =  data[item].diff() / data["days"].diff()
    threshold_new = data[item].loc[data.days==0].values[0]
    for output in ['free.dat','qmin.dat','control.dat']:
        df = pd.read_csv(join(item,output),
                         delim_whitespace = True, 
                         names = ["time","i","r"])
        df['dr'] = df['r'].diff()
        i0=(df['r']-threshold_new).abs().argsort()[0]
        df['time'] -= i0
        df.plot(x='time',y='r', label=labels[output], ax=axes[0])
        df.plot(x='time',y='dr',label=labels[output], ax=axes[1])
    data[data.days>0].plot(x="days", y=item,     style=".", alpha=0.75, label=item, ax=axes[0])
    data[data.days>0].plot(x="days", y="ncases", style=".", alpha=0.75, label=item, ax=axes[1])

axes[0].set_yscale('log')
axes[1].set_yscale('log')
axes[0].set_ylabel("Total cases")
axes[1].set_ylabel("Daily new cases")
axes[0].set_xlabel("")
axes[1].set_xlabel(t0.strftime("Days after %Y-%m-%d"))
axes[0].legend(loc=4)
axes[1].legend().set_visible(False)
axes[0].grid()
axes[1].grid()

plt.savefig(fname, dpi=160)
plt.show()
