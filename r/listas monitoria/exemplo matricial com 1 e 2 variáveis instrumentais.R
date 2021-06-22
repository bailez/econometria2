

library(arm)
library(mvtnorm)
library(lme4)
library(multiwayvcov)
library(clusterSEs)
library(ggplot2)
library(dplyr)
library(haven)
library(plm)
library(stargazer)
library(foreign)
library(lmtest)
library(sandwich)
library(AER)
library(haven)
library(tidyverse)

#EXEMPLO COM BETA OLS
# Montando as matrizes
Y <- matrix(c(8, 5, 4, 3, 1), ncol = 1, nrow = 5, byrow = F)
X <- matrix(c(1, 1, 1, 1, 1, 5, 3, 3, 1, 1, 1, 2, 2,
              3, 4), ncol = 3, nrow = 5, byrow = F)
print(X)
mean(Y)
XT <- t(X)
XTX <- XT %*% X
XTX
XTY <- XT %*% Y
XTY

YT <- t(Y)
invXTX <- solve(XTX)
invXTX

betas <- invXTX %*% XTY
betas



# EXEMPLO COM BETA IV COM UM INSTRUMENTAL
Y <- matrix(c(8, 5, 4, 3, 1), ncol = 1, nrow = 5, byrow = F)
Z <- matrix(c(1, 1, 1, 1, 1, 14, 8, 9, 2, 4, 1, 2, 2,
              3, 4), ncol = 3, nrow = 5, byrow = F)
print(Z)



ZT <- t(Z)
ZTX <- ZT %*% X
ZTX
ZTY <- ZT %*% Y
ZTY

YT <- t(Y)
invZTX <- solve(ZTX)
invZTX

BETAIV<- invZTX %*%ZTY
BETAIV
betas


#Usando o code que aprendemos.

library(AER)
library(haven)
library(tidyverse)

Y1 <- matrix(c(8, 5, 4, 3, 1), ncol = 1, nrow = 5, byrow = F)
X2 <- matrix(c( 5, 3, 3, 1, 1), ncol = 1, nrow = 5, byrow = F)
X3 <- matrix(c( 1, 2, 2, 3, 4), ncol = 1, nrow = 5, byrow = F)

#OLS
ols_reg <- lm(Y1 ~ X2 + X3)
summary(ols_reg)


Z1<-matrix(c( 14, 8, 9, 2, 4), ncol = 1, nrow = 5, byrow = F)
#2SLS
iv_reg = ivreg(Y1 ~ X2 + X3 | Z1 + X3)
summary(iv_reg)
BETAIV





#uSO DE 2 INSTRUMENTOS


#Usando o code que aprendemos.

library(AER)
library(haven)
library(tidyverse)

Y1 <- matrix(c(8, 5, 4, 3, 1), ncol = 1, nrow = 5, byrow = F)
X2 <- matrix(c( 5, 3, 3, 1, 1), ncol = 1, nrow = 5, byrow = F)
X3 <- matrix(c( 1, 2, 2, 3, 4), ncol = 1, nrow = 5, byrow = F)
X4<- matrix(c( 8, 5, 3, 1, 7), ncol = 1, nrow = 5, byrow = F)

#OLS
ols_reg <- lm(Y1 ~ X2 + X3 + x4)
summary(ols_reg)


Z1<-matrix(c( 14, 8, 9, 2, 4), ncol = 1, nrow = 5, byrow = F)
Z2<-matrix(c( 3, 5, 4, 2, 5), ncol = 1, nrow = 5, byrow = F)
#2SLS
iv_reg2 = ivreg(Y1 ~ X2 + X3 + X4 | Z1 + Z2 + X4)
summary(iv_reg2)

# EM TERMOS MATRICIAIS
# Montando as matrizes
Yex <- matrix(c(8, 5, 4, 3, 1), ncol = 1, nrow = 5, byrow = F)
Xex <- matrix(c(1, 1, 1, 1, 1, 5, 3, 3, 1, 1, 1, 2, 2,
              3, 4,8, 5, 3, 1, 7), ncol = 4, nrow = 5, byrow = F)

Zex <- matrix(c(1, 1, 1, 1, 1, 14, 8, 9, 2, 4, 3, 5, 4, 2, 5,8, 5, 3, 1, 7), ncol = 4, nrow = 5, byrow = F)


ZexT <- t(Zex)
ZexTX <- ZexT %*% Xex
ZexTX
ZexTY <- ZexT %*% Yex
ZexTY


invZexTX <- solve(ZexTX)
invZexTX

BETAIVex<- invZexTX %*%ZexTY
BETAIVex
summary(iv_reg2)
