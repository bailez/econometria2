import numpy as np
from numpy.linalg import inv
import pandas as pd
import statsmodels.api as sm
from scipy.stats import t

np.set_printoptions(suppress=True)

#Funções

def get_matrix(v, c, convert=True):
    c = c.split(' ')
    v = v.replace('\n',' ').replace(',','.').split(' ')
    s = (int(len(v)/len(c)),len(c))
    if convert:
        v = np.array(v).astype(float)
    else:
        v = np.array(v)
    matrix = np.reshape(v, s)
    return matrix

def get_confidence_interval(betas, std_errors, vcov, 
                            conf_lv, df):
    t_score = t.ppf(1 - conf_lv, df)
    conf_inter = []
    for i in range(len(betas)):
        upper_bound = betas[i] + t_score*std_err[i]
        lower_bound = betas[i] - t_score*std_err[i]
        conf_inter.append((lower_bound,upper_bound))
    return conf_inter
      
# %% (================== Questão 1 ===================) #

# Dados

cols = 'Y X2 X3'
vals ='''800 2 0,8
1160 4 0,7
1580 6 0,5
2010 8 0,4
1890 7 0,2
2600 12 0,2
2070 11 0,8
1890 10 0,7
1830 9 0,6
1740 8 0,1
1380 6 0,5
1060 4 0,4'''

# Transformando em Matrizes

A = get_matrix(v = vals, c= cols)
Y = A[:,0]
X = A.copy()
X[:,0] = 1

# Modelo a ser estimado
# Y = b1 + b2X2 + b3X3 + vt 

# OLS via Statsmodels
df = pd.DataFrame(A,columns=cols.split(' '))
df_exog = sm.add_constant(df.iloc[:,1:])
model = sm.OLS(df.iloc[:,0], df_exog)
result = model.fit()

# %%  (========= Item A =========) #

# Estimando Beta por meio de Matriz (pg 757 Wooldridge)
# B_hat = (X'X)^-1 X'y

B_hat = inv(X.T @ X) @ X.T @ Y

##  Usando Statsmodels ##
B_hat_sm = result.params

# %% (========= Item B =========) #

# Estimando Soma dos quadrados dos residuos por meio de Matriz (pg 757 Wooldridge)
# SSR = (y - XB_hat)' (Y - XB_hat)
# SSR = u_hat' u_hat
# u_hat = y - XB_hat

u_hat = (Y - X @ B_hat)
SSR = u_hat.T @ u_hat 

##  Usando Statsmodels ##
SSR_sm = sum(np.square(result.resid))

# %% (========= Item C =========) #

# Estimando o R^2 do modelo (pg 38 - 40 Wooldridge)
# SST = Sum(Yi - Y_mean) ^ 2
# SSE - Sum(Yi_hat - Y_mean) ^ 2
# SSR = Sum(u_hat)^2
# SST = SSE + SSR
# R_squared = SSE/SST = 1 - SSR/SST WE

SST = sum(np.square(Y - np.repeat(np.mean(Y),len(Y))))
SSE = SST - SSR
R2 = SSE/SST

##  Usando Statsmodels ##
R2_sm = result.rsquared

# %% (========= Item D =========) #

# Estimando Matriz de Variância-Covariância (pg 759 Wooldridge)
# X'(y - XB_hat) = 0
# X' * u_hat =  0
# sigma2 = SSR/ (N - k)
# Var(u | X) =  sigma_2 * In

sigma_2 = SSR/(X.shape[0] - X.shape[1])
vcov = sigma_2 * inv(X.T @ X)

##  Usando Statsmodels ##
vcov_sm = result.cov_params()

# %% (========= Item E =========) #

# Verifica a significancia dos Betas a nivel 5%
conf_lv = 0.05

# Construindo intervalo de Confiança
# Formula de IC:
#  b1  +/-  (t1-∝/2, n-k) * (erro padrão de b1)
  

std_err = np.sqrt(np.diag(vcov))
conf_interval = get_confidence_interval(betas=B_hat, std_errors=std_err, 
                                        vcov = vcov, conf_lv = conf_lv, 
                                        df = X.shape[0] - X.shape[1])

# Testes de Hipótese, obtendo Estatística T e Valor-P
t_score = B_hat/std_err
p_values = t.sf(abs(t_score),  X.shape[0] - X.shape[1])*2 # duas caudas multiplica por 2

##  Usando Statsmodels ##
conf_interval_sm = result.conf_int()
p_values_sm = result.pvalues

# %% (================== Questão 2 ===================) #

cols = 'Y X2 X3 Dummy'
vals = '''800 2 0,8 1
1160 4 0,7 1
1580 6 0,5 1
2010 8 0,4 1
1890 7 0,2 1
2600 12 0,2 1
2070 11 0,8 0
1890 10 0,7 0
1830 9 0,6 0
1740 8 0,1 0
1380 6 0,5 0
1060 4 0,4 0'''

# Transformando em Matrizes

A = get_matrix(v = vals, c= cols)
Y = A[:,0]
X = A.copy()
X[:,0] = 1

# Y = b1 + b2X2 + b3X3 + b4D + vt

# OLS via Statsmodels
df = pd.DataFrame(A,columns=cols.split(' '))
df_exog = sm.add_constant(df.iloc[:,1:])
model = sm.OLS(df.iloc[:,0], df_exog)
result = model.fit()

# %% (========= Item A =========) #
B_hat = inv(X.T @ X) @ X.T @ Y

##  Usando Statsmodels ##
B_hat_sm = result.params

# %% (========= Item B =========) #
u_hat = (Y - X @ B_hat)
SSR = u_hat.T @ u_hat 

##  Usando Statsmodels ##
SSR_sm = sum(np.square(result.resid))

# %% (========= Item C =========) #
SST = sum(np.square(Y - np.repeat(np.mean(Y),len(Y))))
SSE = SST - SSR
R2 = SSE/SST

##  Usando Statsmodels ##
R2_sm = result.rsquared

# %% (========= Item D =========) #
sigma_2 = SSR/(X.shape[0] - X.shape[1])
vcov = sigma_2 * inv(X.T @ X)

##  Usando Statsmodels ##
vcov_sm = result.cov_params()
# %% (========= Item E =========) #

conf_lv = 0.05
std_err = np.sqrt(np.diag(vcov))
conf_interval = get_confidence_interval(betas=B_hat, std_errors=std_err, 
                                        vcov = vcov, conf_lv = conf_lv, 
                                        df = X.shape[0] - X.shape[1])
t_score = B_hat/std_err
p_values = t.sf(abs(t_score),  X.shape[0] - X.shape[1])*2 # duas caudas multiplica por 2

##  Usando Statsmodels ##
conf_interval_sm = result.conf_int()
p_values_sm = result.pvalues

# %% (================== Questão 3 ===================) #

cols = 'Vila(i) Yi(0) Yi(1) t1'
vals = '''vila1 10 15 5
vila2 15 15 0
vila3 20 30 10
vila4 20 15 -5
vila5 10 20 10
vila6 15 15 0
vila7 15 30 5
average 15 20 5'''

A = get_matrix(v = vals, c= cols, convert=False)
Yi = A[:,1:].astype(float)

# E[Yi(0)] 􀀀 E[Yi(1)] = E[Yi(0) - Yi(1)]
np.mean(Yi[:,0]) - np.mean(Yi[:,1]) == np.mean(Yi[:,0] - Yi[:,1]) 

# %% (================== Questão 4 ===================) #