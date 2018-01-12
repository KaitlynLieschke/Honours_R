# -*- coding: utf-8 -*-
"""
Created on Wed Jan 10 10:17:29 2018

@author: jennyf
"""

import pandas as pd
import numpy as np
from scipy.stats.mstats import theilslopes
import matplotlib.pyplot as plt

from rpy2.robjects import r
def decompose(series, frequency, s_window, **kwargs):
    df = pd.DataFrame(index=series.index)
    #df['date'] = series.index
    series.interpolate(inplace=True)
    s = [x for x in series.values]
    length = len(series)
    s = r.ts(s, frequency=frequency)
    decomposed = [x for x in r.stl(s, s_window, **kwargs).rx2('time.series')]
    df['observed'] = series.values
    df['trend'] = decomposed[length:2*length]
    df['seasonal'] = decomposed[0:length]
    df['residual'] = decomposed[2*length:3*length]
    return df

# Only use midday data? -- NEED TO CHANGE manually below!
midday=True
middaymin=10
middaymax=14

# use offset?
offset=True

df1 = pd.read_csv('asc_hcho1.dat',delim_whitespace=True,parse_dates=[['YYYYMMDD','HHMMSS']])
df2 = pd.read_csv('asc_hcho2.dat',delim_whitespace=True,parse_dates=[['YYYYMMDD','HHMMSS']])

df1['YYYYMMDD_HHMMSS']=pd.to_datetime(df1['YYYYMMDD_HHMMSS'],format='%Y%m%d %H%M%S')
df1.set_index([df1.columns[0]], inplace=True)

df2['YYYYMMDD_HHMMSS']=pd.to_datetime(df2['YYYYMMDD_HHMMSS'],format='%Y%m%d %H%M%S')
df2.set_index([df2.columns[0]], inplace=True)

df = pd.concat([df1,df2])


# calculate difference between instruments where they overlap...
if (offset):
    # date 1: 2007-10-04
    inst0 = df['2007-10-04'].loc[df['instrument']==0]['total_column']
    inst1 = df['2007-10-04'].loc[df['instrument']==1]['total_column']
    
    # need to manually change time for now...
    if (midday):
        diff1=inst0.between_time('10:00:00','14:00:00').mean()-inst1.between_time('10:00:00','14:00:00').mean()
    else:
        diff1=inst0.mean()-inst1.mean()
    
    # date 1: 2007-10-11
    inst0 = df['2007-10-11'].loc[df['instrument']==0]['total_column']
    inst1 = df['2007-10-11'].loc[df['instrument']==1]['total_column']
    if (midday):
        diff2=inst0.between_time('10:00:00','14:00:00').mean()-inst1.between_time('10:00:00','14:00:00').mean()
    else:
        diff2=inst0.mean()-inst1.mean()
    
    # date 1: 2007-10-18
    inst0 = df['2007-10-18'].loc[df['instrument']==0]['total_column']
    inst1 = df['2007-10-18'].loc[df['instrument']==1]['total_column']
    if (midday):
        diff3=inst0.between_time('10:00:00','14:00:00').mean()-inst1.between_time('10:00:00','14:00:00').mean()
    else:
        diff3=inst0.mean()-inst1.mean()
    
    # date 4: 2008-04-03
    inst0 = df['2008-04-03'].loc[df['instrument']==0]['total_column']
    inst1 = df['2008-04-03'].loc[df['instrument']==1]['total_column']
    if (midday):
        diff4=inst0.between_time('10:00:00','14:00:00').mean()-inst1.between_time('10:00:00','14:00:00').mean()
    else:
        diff4=inst0.mean()-inst1.mean()

    # Correct inst=0 (df1) by subtracting offset
    df1['total_column']=df1['total_column']-np.array([diff1,diff2,diff3,diff4]).mean()
    # Re-create combined array
    df = pd.concat([df1,df2])


if (midday):
    df1 = df1.loc[(df1.index.hour >= middaymin) & (df1.index.hour <= middaymax)]
    df2 = df2.loc[(df2.index.hour >= middaymin) & (df2.index.hour <= middaymax)]
    df = df.loc[(df.index.hour >= middaymin) & (df.index.hour <= middaymax)]

tc1=pd.Series(data=df1['total_column']).resample('M').mean()
mm1=pd.DataFrame(tc1)
mm1['yearfrac']=np.arange(0,len(tc1))/12.

ssd1 = decompose(tc1, frequency=12, s_window=35, robust=True, s_degree=0)
mm1['deseas']=ssd1['trend']+ssd1['residual']


tc2=pd.Series(data=df2['total_column']).resample('M').mean()
mm2=pd.DataFrame(tc2)
mm2['yearfrac']=np.arange(0,len(tc2))/12.

ssd2 = decompose(tc2, frequency=12, s_window=35, robust=True, s_degree=0)
mm2['deseas']=ssd2['trend']+ssd2['residual']

tc=pd.Series(data=df['total_column']).resample('M').mean()
mm=pd.DataFrame(tc)
mm['yearfrac']=np.arange(0,len(tc))/12.

ssd = decompose(tc, frequency=12, s_window=35, robust=True, s_degree=0)
mm['deseas']=ssd['trend']+ssd['residual']



s1=theilslopes(mm1['deseas'],mm1['yearfrac'])
s2=theilslopes(mm2['deseas'],mm2['yearfrac'])
mm1['trend']=mm1['yearfrac']*s1[0]+s1[1]
mm2['trend']=mm2['yearfrac']*s2[0]+s2[1]

#plt.plot(df1.index.to_pydatetime(),df1['total_column'],'.')
plt.plot(mm1.index.to_pydatetime(),mm1['total_column'])
plt.plot(mm1.index.to_pydatetime(),mm1['deseas'])
plt.plot(mm1.index.to_pydatetime(),mm1['trend'])

plt.plot(mm2.index.to_pydatetime(),mm2['total_column'])
plt.plot(mm2.index.to_pydatetime(),mm2['deseas'])
plt.plot(mm2.index.to_pydatetime(),mm2['trend'])

#plt.xlim(datetime.date(2007,1,1),datetime.date(2009,1,1))
plt.show()

print(s1[0],s1[2],s1[3])
print(s2[0],s2[2],s2[3])

s=theilslopes(mm['deseas'],mm['yearfrac'])
mm['trend']=mm['yearfrac']*s[0]+s[1]
plt.plot(mm.index.to_pydatetime(),mm['total_column'])
plt.plot(mm.index.to_pydatetime(),mm['deseas'])
plt.plot(mm.index.to_pydatetime(),mm['trend'])
plt.show()

print(s[0],s[2],s[3])
