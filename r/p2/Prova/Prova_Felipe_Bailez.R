library(wooldridge)
library(tidyverse)
library(plm)
library(readxl)
library(AER)
setwd('D:/FEA/Econometria2/r/p2/Prova')

# Felipe Matias Bailez Viana
# Numero USP 10370792

# Prova de Econometria 2



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
## ================================= ## Questão 1 ## ================================= ##

Y <- matrix(c(5,6,4,10,4.5), ncol = 1, nrow = 5, byrow = F)
X <- matrix(c(7.5,10,9,18,11, 
              10,15,12.5,20,11,
              14,13,11,18,23), ncol = 3, nrow = 5, byrow = F)

Z <- matrix(c(10,12.5,11.5,20,10,
              10,15,12.5,20,11,
              18,15,13,16,22), ncol = 3, nrow = 5, byrow = F)

##===========##  item (A) ##===========##

betas <- betas_matrix(Y,X)
reg <- lm(Y ~ X)

##===========##  item (B) ##===========##

betas_iv <- iv_matrix(Y,X,Z)
iv_reg <- ivreg(Y ~ X | Z)

##===========## item (c) ##===========##

cor(X[,1],Z[,1])

# Resposta
# Z_2 não é um instrumento fraco para X2. Como a correlação dos dois é alta, então
# o estimador delta da variavel instrumental terá menos chance de conter viés

##===========## item (D) ##===========##

cor(X[,3],Z[,3])

# Resposta
# Z_4 também não é um instrumento fraco para X4. 0,84 de correlação é alta

##===========## item (E) ##===========##


summary(reg)

# R-squared da OLS normal é 0.8806, e o R-squared ajustado é 0.5225

summary(iv_reg)

# R-squared da IV é 0.8479, e o R-squared ajustado é 0.4621


##===========## item (F) ##===========##

cor(X,Z)
cor(c(reg$residuals),Z)
 
# O modelo com variável instrumental deve ser preferível ao modelo com variável simples
# quando sabemos que uma variavel endogena está correlacionada com o resíduo, e temos
# uma variável Z que tem relação causal (ou seja, correlação) com a variável endógena,
# mas não possui relação causal com a variável de interesse nem é correlacionada com o error.


## ================================= ## Questão 2 ## ================================= ##


data(jtrain)


##===========## item (A) ##===========##

df_2 <- pdata.frame(jtrain, index = dim(jtrain)[1])

reg_2 <- lm(hrsemp ~ d88 + d89 + grant + log(employ), data=df_2)
summary(reg_2)

# A regressao leva a reflexão de que o maior numero de empregados diminui as horas
# de treinamento e que receber a bolsa de treinamento profissional para 1988 pode 
# aumentado em 32.58 vezes a quantidade de horas treinadas em cada empresa


##===========## item (B) ##===========##

reg_2_fe <- plm(hrsemp ~ d88 + d89 + grant + log(employ), 
                data=df_2, model='within', effect='time')
summary(reg_2_fe)


##===========## item (D) ##===========##

#As empresas maiores fornecem menos treinamento aos seus funcionários.


## ================================= ## Questão 3 ## ================================= ##

data(mroz)

##===========## item (A) ##===========##
df_3 <- pdata.frame(mroz)
reg_3 <- lm( log(wage) ~ educ, data = df_3)
summary(reg_3)

# No modelo estimado por OLS é possivel ver que para cada 1 ano a mais de educação
# o salário aumenta em 10%.

##===========## item (B) ##===========##
reg_3_father <- lm(educ ~ fatheduc, data = df_3)
summary(reg_3_father)

# No modelo estimado por OLS é possivel ver que para cada 1 ano a mais de educação
# do pai, a educação do filho aumenta em 0.28 anos.

##===========## item (C) ##===========##

# Se a variavel do pai não é correlacionada com o erro da regressão original,
# e é razoável acreditar que a educação do pai afeta a renda através da educação do filho,
# então nesse caso seria um bom instrumento.

##===========## item (D) ##===========##

ivreg_3 <- ivreg( log(wage) ~ educ | fatheduc, data= df_3)
summary(ivreg_3)
summary(reg_3)

#Beta original cai de 0.1086 para 0.05917 utilizando variavel instrumental

## ================================= ## Questão 4 ## ================================= ##


df_4 <- read_excel('dados questão 4.xlsx')

##===========## item (A) 

rem_reg_pool <- plm(indiceremun ~ desemp,
                data = df_4, model ="pooling" )

summary(rem_reg_pool)

# Ao fazer esta regressão concluimos que o nivel de desemprego alto está associado
# a uma diminuição do nivel de renda.

##===========## item (B) ##===========##

rem_reg_fixed_effects <- plm(indiceremun ~ desemp + factor(year),
                        data = df_4, model ="within", index = c('country') )

summary(rem_reg_fixed_effects)

 
# Com efeitos fixos por país e os anos como dummies é possivel verificar que os salários
# aumentam a medida que os anos aumentam.

##===========## item (C) ##===========##

# Os efeitos fixos nessa equação são justificados para garantir que as particularidades
# do mercado de trabalho no Canadá, Reino Unido e Estados Unidos, assim como de cada ano
# sejam adequadamente comparadas. Faz todo sentido portanto, considerar que as variações
# no reino unido possuem caracteristicas proprias que não existem nos EUA, por isso utilizamos
# os efeitos fixos.

## ================================= ## Questão 5 ## ================================= ##


df_5 <- read_excel('Questao5 dados.xlsx')

##===========## item (A) ##===========##

df_5_piv <- pivot_longer(df_5, cols=starts_with("20"),names_to='year')
names(df_5_piv) <- c('favelas', 'treated', 'year', 'value')

df_5_piv$time = ifelse(df_5_piv$year >= 2016, 1, 0)

df_5_piv$did = df_5_piv$time * df_5_piv$treated


didreg = lm(value ~ treated + time + did, data = df_5_piv)
summary(didreg)


# A regressão DiD nos diz a 1% de significancia que o tratamento diminuiu em 11.16
# vezes a violência nas favelas tratadas no Rio de Janeiro.


## item (B) ##

ggplot(df_5_piv, aes(x=favelas, y=value)) + 
  geom_point(aes(x=favelas,y=value,color=year))

# É possivel observar no gráfico uma clara dispersão a partir das favelas tratadas
# que são aquelas a partir da de numero 300. Uma dispersão da violencia é clara para
# o ano de 2016


## item (C) ##

# Considerando os itens acima a validade é de que removendo as diferenças particulares
# das favelas é possível aproximar um contrafactual para as favelas que são tratadas
# e assim estimar qual é o efeito do tratamento. Com o gráfico e os dados da regressão
# é possível averiguar que a tendencia é de fato visível.