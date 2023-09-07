# -*- coding: utf-8 -*-
"""
Created on Mon May 18 16:42:51 2020

@author: KVENABLE
"""


import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from scipy.stats import linregress

filename = "HMSSedimentOutputandInputFile\XY16JUN20a.txt"
file = open(filename, mode='r')

lines =file.readlines()[:]
result = []
for x in lines:
    # need to insert a dataframe for each of these isolatons
   result.append(x.split())
file.close()  
df=pd.DataFrame(result, columns= ['Time', 'PDX', 'YL1', 'YL2', 'YL3'], dtype=float)

# condition = df['CL1'] > 0
# condition1 = df['CL2'] > 0
# condition2 = df['CL3'] > 0
df2=-1*df[:]
#plt.xscale('log')
plt.xlabel('Time (s)')
plt.ylabel('Discplacement in the y(m)')
plt.title('TSS Advection over Time Location of Deposition')
plt.scatter(df.PDX, df2.YL1, label='YL1')
plt.scatter(df.PDX, df2.YL2, label='YL2')
plt.scatter(df.PDX, df2.YL3, label='YL3')
ax = plt.gca()
#ax.set_ylim([0.1,18000])
ax.set_xlim([0,max(df.PDX)])
plt.legend(loc='best')
print(plt.show()) 
plt.close()