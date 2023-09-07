# -*- coding: utf-8 -*-
"""
Created on Mon Apr  6 13:46:40 2020

@author: KVENABLE
"""
import geogtools as gts
import pandas as pd
import numpy as np
import sqlalchemy as sq
content = dir(gts)
print(content)
print(gts('Brier'))
df =  gts.GeogTool.buildNHDplus(self, setNHDplus_attribute=0)

