import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA

data = pd.read_csv('/home/saf537/Documents/GWR_hedonicP_MDE/Data/RE_MDE.csv')

""" Data standardizing etc."""



""" Exploratory plots """
plt.scatter(np.log(data['Area']),np.log(data.Price))
plt.xlabel("Area - log")
plt.ylabel("Price - log")
#plt.show()
plt.savefig('/home/saf537/Documents/GWR_hedonicP_MDE/Plots/logArea_logprice.png')


""" Feature selection """
pca = PCA(n_components=2)
X = np.array(data[['Price','Area']].iloc[:3])
pca.fit(X)


""" Linear regression """
