library(wooldridge)
library(plm)
library(readxl)
library(AER)

## ====== Questão 1 ==== ##


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

rsquared_matrix <- function(Y,X,B){
  cols = rep(1,dim(X)[1])
  X <- cbind(cols, X)
  u <- Y - X %*% B
  ut <- t(u)
  SSR <- ut %*% u
  Yt <- t(Y)
  SST <- Yt %*% Y
  SSE <- SST - SSR
  R_squared <- 1 - SSR/SST
  return(R_squared)
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

Y <- matrix(c(10,11.6,8,20.1,9), ncol = 1, nrow = 5, byrow = F)
X <- matrix(c(15,20,18,35,22,
              20,30,25,40,35), ncol = 2, nrow = 5, byrow = F)

Z <- matrix(c(20,25,23,37,18,
              20,30,25,40,35), ncol = 2, nrow = 5, byrow = F)

##  item (a) ##
betas <- betas_matrix(Y,X)
reg <- lm(Y ~ X)

##  item (b) ##
betas_iv <- iv_matrix(Y,X,Z)
iv_reg = ivreg(Y ~ X | Z)

## item (c) ##

cor(x[,1],Z[,1])

# A correlação entre Z e X é alta, então o instrumento não é fraco.
# O instrumento fraco seria 

## item (D) ##
r_squared <- rsquared_matrix(Y,X,betas)


## item (E) ##


## ====== Questão 2 ==== ##


data(mathpnl)


## item (A) ##

df <- pdata.frame(mathpnl, index = dim(mathpnl)[1])
reg_pool <- plm(math4 ~ y94 + y95 + y96 + y97 + y98 + log(rexpp) + log(enrol) + lunch,
           data = df, model ="pooling" )
coeftest(reg_pool, vcov. = vcovHC)
summary(reg_pool)

## item (B) ##

## item (C) ##

reg_fixed_effects <- plm(math4 ~ y94 + y95 + y96 + y97 + y98 + log(rexpp) + log(enrol) + lunch,
                  data = df, model ="within", effect="twoways")
coeftest(reg_fixed_effects, vcov. = vcovHC)
summary(reg_fixed_effects)

## item (D) ##


## ====== Questão 3 ==== ##

data(bwght)

## item (A) ##
df_bwgth <- pdata.frame(bwght)
wght_reg <- lm( log(bwght) ~ packs, data = df_bwgth)

## item (B) ##
iv_wght_reg <- ivreg( log(bwght) ~ packs | cigtax, data = df_bwgth)

## item (C) ##
reduced_reg <- lm( log(bwght) ~ cigprice, data=df_bwgth)
first_stage <- lm( packs ~ cigprice, data=df_bwgth)

packs_fit <- fitted(first_stage)

second_stage <- lm( log(bwght) ~ packs_fit, data=df_bwgth)

## ====== Questão 4 ==== ##

setwd('D:/FEA/Econometria2/r/p2')
df_4 <- read_excel('dados questão 4.xlsx')

## item (A) ##
rem_reg_pool <- plm(indiceremun ~ desemp,
                data = df_4, model ="pooling" )

## item (B) ##

rem_reg_fixed_effects <- plm(indiceremun ~ desemp + factor(year),
                        data = df_4, model ="within", effect = "time" )


## ====== Questão 5 ==== ##

df_5 <- read_excel('dadosquestao5.xlsx')

## item (A) ##

mydata$time = ifelse(mydata$year >= 1994, 1, 0)
mydata$treated = ifelse(mydata$country == "E" |
                          mydata$country == "F" |
                          mydata$country == "G", 1, 0)

# Create an interaction between time and treated. We will call this interaction
#'did'.

mydata$did = mydata$time * mydata$treated

# Estimating the DID estimator

didreg = lm(y ~ treated + time + did, data = mydata)
summary(didreg)

didreg1 = lm(y ~ treated*time, data = mydata)
summary(didreg1)
library(tidyverse)
## item (B) ##
df_5 < - pivot_longer(df_5, cols=starts_with("crime"))
## item (C) ##
