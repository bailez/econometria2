#          Prova de Econometria 2    - 14/05/2021
#
#   Nome: Felipe Matias Bailez Viana
#   nUSP: 10370792
#
#


library(tidyverse)
library(wooldridge)
library(lmtest)
library(sandwich)
library(haven)

### ================================  1  ================================ ###

N = 6
k = 3

Y <- matrix(c(3, 14, 20, 22, 23, 33),
            ncol = 1, 
            nrow = N)

X <- matrix(c(rep(1,N),
              0, 2, 2, 4, 4, 6,
              0, 4, 8, 4, 8, 10),
            ncol = k,
            nrow = N)

### ==  A == ###

Xt <- t(X)
XtX <- Xt %*% X
inv_XtX <- solve(XtX)

### ==  B == ###

XtY <- Xt %*% Y
B <- inv_XtX %*% XtY

#    Equação da Regressão Estimada
#   
#        Y = 3.688073 + 3.114679 * X_1 + 1.082569 * x_2
#

### ==  C == ###

u <- Y - X %*% B
ut <- t(u)
SSR <- ut %*% u
sigma <- (SSR/(N - k))[1]
varcov <- sigma*inv_XtX
ep <- sqrt(diag(varcov))
xi <- 2
tbeta_2 <- (B[xi] - 0)/ep[xi]

# É possível rejeitar H0 de que Beta 2 é igual a 0 uma vez que o valor T = 5.54118 é maior que t.95 


### ==  D == ###

s <- diag(c(u))
s2 <- s %*% s
vcov_test <- (N / N - k) * inv_XtX %*% ( Xt %*% s2 %*% X) %*% inv_XtX # Robust test Beta

# Confidence Interval
xi <- 3
conf_lv <- 0.05
sig <- qt(conf_lv, N - k, lower.tail = TRUE)
ic_sup <- B[xi] + vcov_test[xi, xi]*sig
ic_inf <- B[xi] - vcov_test[xi, xi]*sig
ic <- cbind(ic_inf,ic_sup)

# A estimação de beta 3é estatísticamente significante, pois o intervalo de confiança 
# abrange entre 0.7240192 e 1.441118 incluindo beta3.
# O alcance do IC é definido por uma relação entre erro padrão e o tamanho da amostra.


### ==  E == ###

# R-squared

Yt <- t(Y)
SST <- Yt %*% Y
R2 <- 1 - SSR/SST

# A estimação do modelo linear mostra que variações em Xi explicam 99,69769% das variações em Y


### ================================  2  ================================ ###

data(wagepan)

### ==  A == ###

model1 <- lm(lwage ~ married + educ + hours, data = wagepan)
summary(model1)

### ==  B == ###

model2 <- lm(lwage ~ married + factor(year) + factor(year)*educ + educ + hours, 
             data = wagepan )
summary(model2)

### ==  C == ###

robust = coeftest(model2, vcov.=vcovHC)


# Os erros padrões com robustez consideram os efeitos de heteroscedasticidade dos erros
# ou autocorrelação por este motivo permitem estimar com menos variança


### ================================  3  ================================ ###

N = 15
k = 2

P <- matrix(seq(1:15), nrow=15)
Y_i <- matrix(c(10, 12, 8, 5, 7, 4, 5, 11, 5, 4, 9, 10, 4, 13, 5,
                6, 4, 5, 4, 3, 2, 6, 4, 3, 8, 6, 5, 3, 8, 1),
              ncol = k, 
              nrow = N)


P_even <- P[lapply(P, "%%", 2) == 0]
P_odd <- P[lapply(P, "%%", 2) == 1]

Y_1_even <- Y_i[P_even]
Y_2_even <- Y_i[P_even + N]
Y_1_odd  <- Y_i[P_odd]
Y_2_odd  <- Y_i[P_odd + N]

### ==  A == ###

ATE <- mean(Y_i[,1]) - mean(Y_i[,2])


### ==  B == ###
ATT <- mean(Y_1_even) - mean(Y_2_even)

### ==  C == ###
ATU <- mean(Y_1_odd) - mean(Y_2_odd)
SDO <- mean(Y_1_even) - mean(Y_2_odd)

### ==  D == ###

# Todas as estimativas apresentam efeito positivo nos individuos
# Onde temos que ATE = 2.933, ATT = 3.428, ATU = 2.5, SDO = 4.30
# Logo é correto afirmar que o medicamento pode prolongar a vida destes pacientes


### ================================  4  ================================ ###


read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}
titanic<- read_data("titanic.dta") %>% 
  mutate(d = case_when(class == 1 ~ 1, TRUE ~ 0))

### ==  A == ###


e_y_1 <- titanic %>% 
  filter(d == 1) %>%
  pull(survived) %>% 
  mean()

e_y_0 <- titanic %>% 
  filter(d == 0) %>%
  pull(survived) %>% 
  mean()

SDO <- e_y_1 - e_y_0

### ==  B == ###

#Como o SDO é composto pelo ATE, viés de seleção e viés de tratamento heterogeneo o
#resultado do ATE ponderad será semelhante ao SDO