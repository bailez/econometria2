
library("tydverse")
#Montando as matrizes
Y <- matrix(c(8, 5, 4, 3, 1), ncol = 1, nrow = 5, byrow = F);
X <- matrix(c(1, 1, 1, 1, 1, 5, 3, 3, 1, 1, 1, 2, 2,
                 3, 4), ncol = 3, nrow = 5, byrow = F)
print(X)
print(Y)
mean(Y)
ybarra<- mean(Y) 


#Transposta de XT
XT <- t(X)
#calculando XTx
XTX<-XT %*% X
XTX
#Calculando XTY
XTY<- XT%*%Y
XTY

#Calculando YT
YT<- t(Y)

invXTX <- solve(XTX)
invXTX


betas <- invXTX %*% XTY
betas

#cALCULANDO O RESÍDUO
XBETA<- X %*% betas
ECHAPEU<- Y - XBETA
#achando variância total
ECHAPEUT<- t(ECHAPEU)
n<- nrow(Y)
k<-3
SIGMA2<- as.numeric((ECHAPEUT%*%ECHAPEU)/(n-k))
print(SIGMA2)

#matriz de variância e covariância

mavcov<-SIGMA2* invXTX
print(mavcov)
#obtendo o I.C
ep<- sqrt(diag(mavcov))
ep
ep_beta_1<- ep[1]
print(ep_beta_1)
ep_beta_2<- ep[2]
print(ep_beta_2)
ep_beta_3<- ep[3]
cbind(ep_beta_1,ep_beta_2,ep_beta_3)

#obtendo primeiro o teste de significância
# Valor tabelados de t para 1%, 5% e 10%
gl <- (1000 - k)
qt(0.005, gl)
qt(0.025, gl)
qt(0.05, gl)
tabelado1 <- abs(qt(0.005, gl))
tabelado1
tabelado5 <- abs(qt(0.025, gl))
tabelado10 <- abs(qt(0.05, gl))

#Se ele é significante :
tbeta1 <- ((betas[1] - 0)/ep_beta_1)
tbeta1
tbeta2 <- ((betas[2] - 0)/ep_beta_2)
tbeta2
tbeta3 <- ((betas[3] - 0)/ep_beta_3)
tbeta3

i.c.inferior<- betas[1] - ep_beta_1*tabelado1
i.c.superior<-betas[1]+ ep_beta_1*tabelado1
cbind(i.c.inferior, i.c.superior)


#obtendo os r^2

SQE <- t(betas)%*%XTY - n*(ybarra^2)
YT<-t(Y)
YTY<- YT %*% Y
SQR<- YTY - t(betas)%*%XTY
SQT<- YTY  - n*(ybarra^2)

print(SQE)
print(SQT)
print(SQR)

R2<- SQE/SQT
print(R2)

