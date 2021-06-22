
##=============## VARIAVEIS INSTRUMENTAIS ##=============##

library(AER) #iv_reg

iv_matrix <- function(Y,X,Z) {
  cols = rep(1,dim(X)[1])
  X <- cbind(cols, X)
  Z <- cbind(cols, Z)
  ZT <- t(Z)
  ZTX <- ZT %*% X
  ZTY <- ZT %*% Y
  YT <- t(Y)
  invZTX <- solve(ZTX)
  betas <- invZTX %*%ZTY
  
  return(betas)
}

betas_matrix <- function(Y,X) {
  cols = rep(1,dim(X)[1])
  X <- cbind(cols, X)
  XT <- t(X)
  XTX <- XT %*% X
  XTY <- XT %*% Y
  YT <- t(Y)
  invXTX <- solve(XTX)
  betas <- invXTX %*% XTY
  return(betas)
}


## Para apenas 1 instrumento ##

Y <- matrix(c(8, 5, 4, 3, 1), ncol = 1, nrow = 5, byrow = F)

X <- matrix(c(5, 3, 3, 1, 1, 
              1, 2, 2, 3, 4), 
            ncol = 2, nrow = 5, byrow = F)

Z <- matrix(c(14, 8, 9, 2, 4,  # Instrumento
              1, 2, 2, 3, 4),
            ncol = 2, nrow = 5, byrow = F)


betas <- betas_matrix(Y,X)
betas_iv <- iv_matrix(Y,X,Z)

ols <- lm(Y ~ X)
print(ols)
print(betas)

iv_reg = ivreg(Y ~ X | Z)
print(iv_reg)
print(betas_iv)


## Para 1 ou mais instrumentos ##

Y <- matrix(c(8, 5, 4, 3, 1), ncol = 1, nrow = 5, byrow = F)
X <- matrix(c(5, 3, 3, 1, 1, 
              1, 2, 2, 3, 4,
              8, 5, 3, 1, 7),
            ncol = 3, nrow = 5, byrow = F)

Z <- matrix(c(14, 8, 9, 2, 4, # Instrumento 1
              3, 5, 4, 2, 5, # Instrumento 2
              8, 5, 3, 1, 7), 
            ncol = 3, nrow = 5, byrow = F)

betas <- betas_matrix(Y,X)
betas_iv <- iv_matrix(Y,X,Z)

ols <- lm(Y ~ X)
print(ols)
print(betas)

iv_reg = ivreg(Y ~ X | Z)
print(iv_reg)
print(betas_iv)