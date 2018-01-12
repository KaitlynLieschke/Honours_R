# -*- coding: utf-8 -*-
"""
Created on Wed Jan 10 10:17:29 2018

@author: jennyf
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.backends.backend_pdf import PdfPages

# Make boxplots?
bp = False

# Make histograms?
hi = True

# Also show combined data set (both inst)?
combined = False

# Save
if (bp):
    pp_bp = PdfPages('hcho_boxplots.pdf')
if (hi):
    pp_hi = PdfPages('hcho_histograms.pdf')

# Read data files
df1 = pd.read_csv('asc_hcho1.dat',delim_whitespace=True,parse_dates=[['YYYYMMDD','HHMMSS']])
df2 = pd.read_csv('asc_hcho2.dat',delim_whitespace=True,parse_dates=[['YYYYMMDD','HHMMSS']])

# Convert date & time to index
df1['YYYYMMDD_HHMMSS']=pd.to_datetime(df1['YYYYMMDD_HHMMSS'],format='%Y%m%d %H%M%S')
df1.set_index([df1.columns[0]], inplace=True)

df2['YYYYMMDD_HHMMSS']=pd.to_datetime(df2['YYYYMMDD_HHMMSS'],format='%Y%m%d %H%M%S')
df2.set_index([df2.columns[0]], inplace=True)

# Combined version
df = pd.concat([df1,df2])

# Hours to use for histogram plot - no data outside these
bins=np.arange(5,18,1)

# One plot per month
for month in range(1,13):
    
    # Extract data for this month
    monthly1 = df1.loc[df1.index.month == month]
    monthly2 = df2.loc[df2.index.month == month]
    monthlyall = df.loc[df.index.month == month]
            
    # Make boxplots of HCHO total column by hour
    if (bp):
        
        plt.figure()

        # Show inst 1 and inst 2 side by side
        monthly=pd.concat([monthly1,monthly2])
        sns.boxplot(monthly.index.hour, monthly['total_column'],hue=monthly['instrument'])
        plt.title("Month = {}".format(month))
        plt.xlabel("Hour of Day")
        plt.ylabel("HCHO Total Column (molec/cm2)")
        # plt.ylim(0,5e16) # Toggle to zoom
        #plt.show()

        # Plot combined (inst 0+1) dataset        
        if (combined): 
            sns.boxplot(monthlyall.index.hour, monthlyall['total_column'])
            plt.title("Combined dataset, Month = {}".format(month))
            plt.xlabel("Hour of Day")
            plt.ylabel("HCHO Total Column (molec/cm2)")
            #plt.ylim(0,5e16) # Toggle to zoom
            #plt.show()
            
        pp_bp.savefig()

    # Make histograms of measurements by hour
    if (hi):
        
        plt.figure()
                
        monthly1['hour']=monthly1.index.hour
        monthly2['hour']=monthly2.index.hour
        x,b,p=plt.hist([monthly1['hour'],monthly2['hour']],bins=bins,normed=1,label=[0,1])
        plt.title("Month = {}".format(month))
        plt.xlabel("Hour of Day")
        plt.ylabel("Number of observations")
        plt.legend(title="instrument")
        #plt.show()
        
        pp_hi.savefig()

if (bp):
    pp_bp.close()
if (hi):
    pp_hi.close()