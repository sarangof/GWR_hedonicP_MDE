# -*- coding: utf-8 -*-
"""
Created on Mon Oct 26 11:43:31 2015

@author: Sara
"""
import pandas as pd
import numpy as np

#import matplotlib.pyplot as plt

# Reading the data, cleaning it
RE_M = pd.read_csv('OIME.csv')
RE_M = RE_M[['Codigo','Precio','Tipo.de.Predo','Estado','Stratum','Area','Area.Lote','Antiguedad','Parqueadero','Cuarto.Util','coordY', 'coordX', 'DistCentro','D_roads','d_pob_05','D_Green',"Dist_tp"]]
RE_M.columns = ['ID','Price','Type','Status','Stratum','Area','Area_Lot','Age','Parking','Util','coordY', 'coordX', 'DistCentr','Dist_Road','Density',"Dist_Green","Dist_tp"]
RE_M['coordX'] = RE_M.coordX.astype(np.float64)
pub_transp = pd.read_csv("public_transport.csv")



# Removing outliers
cols = ['Price','Area','Area_Lot','Age']
for col in cols:
    col_zscore = col + '_zscore'
    RE_M[col_zscore] = (RE_M[col] - RE_M[col].mean())/RE_M[col].std(ddof=0)

for col in cols:
    col_zscore = col + '_zscore'
    RE_M = RE_M[(RE_M[col_zscore] < 3) & (RE_M[col_zscore] > -3)]

RE_M[['Dist_Road',"Dist_Green","Dist_tp"]].multiply(1000)

RE_M = RE_M.drop(['Price_zscore','Area_zscore','Area_Lot_zscore', 'Age_zscore'],1)
RE_MDE = RE_M

# Create variables 

RE_MDE = RE_MDE.dropna()

RE_MDE.to_csv("RE_MDE.csv") 