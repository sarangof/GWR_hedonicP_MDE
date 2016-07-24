# -*- coding: utf-8 -*-
"""
Created on Mon Nov 09 19:34:22 2015

@author: Sara
"""

import pandas as pd

pub_transp = pd.read_csv("Geographical Data\estaciones_metro.csv",sep=";",decimal=',')
pub_transp = pub_transp[['FID', 'OBJECTID_1', 'OBJECTID', 'Estacion_1', 'Estacion_2','Estacion_5', 'Estado', 'Sist_1','Sist_2', 'Sist_3','coordX1','coordY1']]
pub_transp.columns = ['FID', 'OBJECTID_1', 'OBJECTID', 'Estacion_1', 'Estacion_2','Estacion_5', 'Estado', 'Sist_1','Sist_2', 'Sist_3','coordX','coordY']
pub_transp.to_csv("public_transport.csv")