# -*- coding: utf-8 -*-
"""
Created on Tue Apr 21 15:35:11 2020

@author: KVENABLE
"""

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from scipy.io import FortranFile

filename = "HMSSedimentOutputandInputFile\conc10APR20a.txt"
file = open(filename, mode='r')

lines =file.readlines()[1:]
result = [float(x)]
# result1=  []
# result2 = []
# result3 = []
# def newarray():
#     na = np.array()
#     return newarray
data=pd.read_csv(filename, sep =',', skiprows=0)
data_array=data.values
   
np1=np.array(data_array)
#np_2d=np.array(file,dtype=str)

#df=pd.DataFrame(result, columns= ['Time', 'CL1', 'CL2', 'CL3'])
for x in lines:
    # need to insert a dataframe for each of these isolatons
   result.append(x.split() [0])
   result.append(x.split() [1:2])
   result.append(x.split() [2:3])
   result.append(x.split() [3:4])
file.close()  
#df=pd.DataFrame(data_array, columns= ['Time', 'CL1', 'CL2', 'CL3'])
# for item in result:
#     result.append(float(item))
# file.close()      
# for item in result_1:    
#     result_1.append(float(item))
# file.close()      
# for item in result_2:    
#     result_2.append(float(item))
# file.close()      
# for item in result_3:    
#     result_3.append(float(item))
# file.close()      
    
# def df1():
#     df1 = np.array()
#     return df1    
# #np_data = np.savetxt(filename, df1.to_numpy(), skiprows=1, fmt="%f %f %f %f")
# data = pd.read_csv(filename, skiprows=1, delimiter=',' , sep = '')

# df1 = np.full_like(data, fill_value=float, dtype=None, order = 'F', shape=None)
#data_array = data.values
#print(data.head())
#plt.plot(result[:-1], data.index)
plt.xlabel('Time (x)')
plt.ylabel('Concentration (mg/L)')
plt.title('TSS Advection over Time')
plt.plot(result[1)
print(plt.show())
plt.grid(True)
plt.close()
file.close()