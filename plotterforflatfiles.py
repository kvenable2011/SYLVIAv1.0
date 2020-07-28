# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 12:02:23 2020

@author: KVENABLE
"""


import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

filename = "HMSSedimentOutputandInputFile\conc10APR20a.txt"
file = open(filename, mode='r')
#text = file.read()
#print(text)
#file.close()

i=1
data_2 = pd.read_csv(filename, skiprows=1, sep=',' )

# fig, ct = plt.subplots()
# ct.plot(data_2)
# plt.xlabel('Time (x)')
# plt.ylabel('Concentration (mg/L)')
# plt.title('TSS Advection over Time')
# print(plt.show())
# plt.grid(True)
# plt.close()
df=data_2.head()
print(df)
print(tuple(data_2))
data_array = data_2.values
#print(data_array)
print(type(data_array[0][0]))

#print(time)
#print(data[0,1])

""" plotter for data tables"""
#plot.
# plt.xlabel('Time (x)')
#plot.ylabel('Concentration (mg/L)')
#plot.title('TSS Advection over Time')
#plot.show()
#plot.grid(True)
#plot.close()
""" Header from txt np.arrays """
data = np.loadtxt(file, delimiter=',', skiprows=0, dtype=str)
#ime = np.array(data[:][1])
col_1=(data[0][1:5])
print(col_1)
col_2=(data[0][5:8])
print(col_2)
col_3 = (data[0][8:11])
print(col_3)
col_4 =np.array(data[0][11:14])
print(col_4)
print(type(col_4))
# df1=(data[0][2])
# print(df1)
# print(time)
#print(data.count("0"))
#print(data)
file.close()