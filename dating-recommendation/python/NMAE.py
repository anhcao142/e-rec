# -*- coding: utf-8 -*-
"""
Created on Wed Mar 16 15:29:24 2016

@author: PHUONGANH
"""

import pandas as pd
import numpy as np
from sklearn.metrics import mean_absolute_error
from sklearn.metrics import mean_squared_error

def NMAE(): 
    print 'Calculating result...'
    result = pd.read_csv('BELLKOR.csv')
    mae = mean_absolute_error(result['rating'],result['predict'])
    nmae = mae/(max(result['rating'])-min(result['rating']))
    RMSE = mean_squared_error(result['rating'], result['predict'])**0.5
    print nmae
    print mae  
    print RMSE
    return nmae
    
NMAE()