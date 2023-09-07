# -*- coding: utf-8 -*-
"""
Created on Fri Apr 24 09:48:13 2020

@author: KVENABLE
"""


import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from scipy.stats import linregress

filename = "mass22JAN21c.txt"
file = open(filename, mode='r')

lines =file.readlines()[1:]
result = []
for x in lines:
    # need to insert a dataframe for each of these isolatons
   result.append(x.split())
file.close()  
df=pd.DataFrame(result, columns= ['Time', 'ML1', 'ML2', 'ML3', 'TotVol', 'TotalDepth', 'Concentration'], dtype=float)

# condition = df['CL1'] > 0
# condition1 = df['CL2'] > 0
# condition2 = df['CL3'] > 0

#plt.xscale('log')
#plt.xlabel('Time (x) log')
plt.ylabel('Mass (g)')
plt.title('TSS Advection over Time w/ Deposition')
plt.scatter(df['Time'], df['ML1'], label='CL1')
plt.scatter(df['Time'], df['ML2'], label='CL2')
plt.scatter(df['Time'], df['ML3'], label='CL3')
ax = plt.gca()
#ax.set_ylim([10000,5000000])
ax.set_xlim([1,max(df.index)])
plt.legend(loc='best') 
print(plt.show())
plt.close()

plt.figure()
plt.xlabel('Time (x)')
plt.ylabel('Mass (g)')
plt.title('TSS Advection over Time w/ Deposition')
plt.plot(df['Time'], df['ML1'], 'ro', label = 'Layer 1')
plt.plot(df.Time, df.ML2, 'bs', label = 'Layer 2')
plt.plot(df.Time, df.ML3, 'go', label = 'Layer 3')
plt.legend(loc='best')  
ax = plt.gca()
#ax.set_ylim([10000,5000000])
ax.set_xlim([0.1,max(df.index)])  
file.close()

