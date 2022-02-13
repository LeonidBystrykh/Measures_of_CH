#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar 24 14:23:56 2021

@author: leonid
"""

import matplotlib.pyplot as plt, numpy as np, pandas as pd
from skbio.diversity.alpha import gini_index, shannon, pielou_e, simpson
import statsmodels.formula.api as smf
from statsmodels.graphics.regressionplots import plot_regress_exog, plot_partregress
import seaborn as sns
plt.style.use('ggplot')
#%%
"This is the major simulation block, which tests robustness of the fit"
Ich_data,Gin_data,Sim_data,Shi_data, Pie_data=[],[],[],[],[]
res_ols, res_gls, res_Shas, res_Sha=[],[],[],[]
for n in range(50): #careful with big numbers, it can be slow
    if n%10==0:
        print(n)
    for i in range(50,500,25): 
        for j in range(1,21):
            x=np.random.beta(21-j,j, size=i)
            xn=x/sum(x)
            Xch=[x for x in xn if x>0.005]
            Ich=sum(Xch)
            Ich_data.append(Ich) #the final Ch index
            Gin=gini_index(x)
            Gin_data.append(Gin)
            Sim=simpson(x)
            Sim_data.append(Sim)
            Shi_data.append(shannon(x))
            Pie=pielou_e(x)
            Pie_data.append(Pie)       
    df = pd.DataFrame(data=[Ich_data, Gin_data, Pie_data, Shi_data, Sim_data]).T
    df.columns=["CH", "Gini", "Pielou", "Shannon", "Simpson"]
    results = smf.ols('CH ~ Shannon+ Simpson+ Gini + Pielou', data=df).fit()
    resShaS=smf.ols('CH ~ Shannon+Simpson', data=df).fit()
    resSh=smf.ols('CH ~ Shannon', data=df).fit()
    resgls = smf.gls('CH ~ Shannon+ Simpson+ Gini + Pielou', data=df).fit()
    res_ols.append(results.rsquared)
    res_gls.append(resgls.rsquared)
    res_Shas.append(resShaS.rsquared)
    res_Sha.append(resSh.rsquared)
#%%
sum_df=pd.DataFrame(data=[res_ols,res_gls, res_Shas, res_Sha]).T 
sum_df.columns=["R2 full OLS", "R2 full GLS","R2 Shann+Simp", "R2 Shannon"]
#%%
sns.boxplot(data=sum_df)
plt.title("Replication of the R-squared using 4 different regression models")
#%%
plt.subplot(221)
sns.scatterplot("CH", "Shannon", data=df)
plt.subplot(222)
sns.scatterplot("CH", "Simpson", data=df)
plt.subplot(223)
sns.scatterplot("CH", "Gini", data=df)
plt.subplot(224)
sns.scatterplot("CH", "Pielou", data=df)
#%%
print(results.params)
#%%
print("Full model R2: ", results.rsquared)
print("Full gls R2: ", resgls.rsquared)
print("Sha+Sim modR2: ", resShaS.rsquared)
print("Shannon onlR2: ", resSh.rsquared)
#%%
# Inspect the results
print(results.summary())
#%%
plot_regress_exog(results, 'Shannon')
#%%
"plot predicted values"
predicted=results.predict(df)
plt.scatter(df["CH"],predicted, s=1)
plt.xlabel("observed CH")
plt.ylabel("predicted CH")
#%%
'plot regression'
plot_partregress("CH","Shannon", ["Simpson", "Pielou", "Gini"],data=df, obs_labels=False)
