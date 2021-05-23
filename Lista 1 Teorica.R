# montando a matriz y
y <- matrix(c(800, 1160, 1580, 2010, 1890, 2600, 2070, 1890, 1830, 1740, 
              1380, 1060), ncol = 1, nrow = 12, byrow = F)

# montando a matriz x
x2 <- matrix(c(2,4,6,8,7,12,11,10,9,8,6,4), ncol=1, nrow=12, byrow=F)
x3 <- matrix(c(.8,.7,.5,.4,.2,.2,.8,.7,.6,.1,.5,.4), ncol=1, nrow=12, byrow=F)
x <- matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,X2,X3), ncol=3, nrow=12, byrow=F)

ybarra <- mean(y)

xt <- t(x)

xtx <- xt %*%x

xty <- xt %*%y

invxtx <- solve(xtx)

betas <- invxtx %*% xty
betas

#Conferindo
regressao1<-(lm(y~x2+x3))
coef(regressao1)
summary(regressao1)

#1 - B
xbeta <- x %*% betas
ECHAPEU <- y - xbeta
ECHAPEUT <- t(ECHAPEU)
Soma_quadrados_residuos <-  ECHAPEUT %*% ECHAPEU

# 1 - C
n = nrow(y)
SQE <- t(betas) %*% xty - n * (ybarra^2)
yt <- t(y)
yty <- yt %*% y
SQR <- yty - t(betas) %*% xty
SQT <- yty - n * (ybarra^2)
R2 <- SQE/SQT
R2

# 1 - D
# cALCULANDO O RESÍDUO
xbeta <- x %*% betas
ECHAPEU <- y - xbeta
# achando variância total
ECHAPEUT <- t(ECHAPEU)
n <- nrow(y)
k <- 3
SIGMA2 <- as.numeric((ECHAPEUT %*% ECHAPEU)/(n - k))

# matriz de variância e covariância
mavcov <- SIGMA2 * invxtx
mavcov

#1 - E
ep <- sqrt(diag(mavcov))
ep_beta_1<-ep[1]
ep_beta_2<-ep[2]
ep_beta_3<-ep[3]
cbind(ep_beta_1, ep_beta_2, ep_beta_3)

gl <- n-k-1
abs(qt(0.025, gl))

tbeta1<-((betas[1])/ep_beta_1)
tbeta1
tbeta2<-((betas[2])/ep_beta_2)
tbeta2
tbeta3<-((betas[3])/ep_beta_3)
tbeta3
#conferindo
coeftest(regressao1)
#todas são significantes a 95%

#2 - A

# montando a matriz y
y <- matrix(c(800, 1160, 1580, 2010, 1890, 2600, 2070, 1890, 1830, 1740, 
              1380, 1060), ncol = 1, nrow = 12, byrow = F)

# montando a matriz x
x4 <- matrix(c(1,1,1,1,1,1,0,0,0,0,0,0), ncol=1, nrow=12, byrow=F)
x <- matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,x2,x3,x4), ncol=4, nrow=12, byrow=F)
x
ybarra <- mean(y)

xt <- t(x)

xtx <- xt %*%x

xty <- xt %*%y

invxtx <- solve(xtx)

betas <- invxtx %*% xty
betas

#conferindo:
regressao1<-(lm(y~x2+x3+x4))
summary(regressao1)
coef(regressao1)

#2 - B
xbeta <- x %*% betas
ECHAPEU <- y - xbeta
ECHAPEUT <- t(ECHAPEU)
Soma_quadrados_residuos <-  ECHAPEUT %*% ECHAPEU

#2 - C
n = nrow(y)
SQE <- t(betas) %*% xty - n * (ybarra^2)
yt <- t(y)
yty <- yt %*% y
SQR <- yty - t(betas) %*% xty
SQT <- yty - n * (ybarra^2)
R2 <- SQE/SQT
R2

#2 - D
xbeta <- x %*% betas
ECHAPEU <- y - xbeta
# achando variância total
ECHAPEUT <- t(ECHAPEU)
n <- nrow(y)
k <- 4
SIGMA2 <- as.numeric((ECHAPEUT %*% ECHAPEU)/(n - k))

# matriz de variância e covariância
mavcov <- SIGMA2 * invxtx
I <- matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), ncol = 4, nrow = 4, byrow = F) 
mavcov_robusta <- mavcov %*% I
mavcov_robusta

#2 - E
ep <- sqrt(diag(mavcov))
ep_beta_1<-ep[1]
ep_beta_2<-ep[2]
ep_beta_3<-ep[3]
ep_beta_4<-ep[4]
cbind(ep_beta_1, ep_beta_2, ep_beta_3, ep_beta_4)

gl <- n-k-1
abs(qt(0.025, gl))

tbeta1<-((betas[1])/ep_beta_1)
tbeta1
tbeta2<-((betas[2])/ep_beta_2)
tbeta2
tbeta3<-((betas[3])/ep_beta_3)
tbeta3
tbeta4<-((betas[4])/ep_beta_4)
tbeta4
#conferindo
coeftest(regressao1)
#todas são significantes a 95%


# Questão 3 Como é possível ver na amostra se observarmos os eventos de forma 
#sequencial (i= 1,2,3,4,5,6,7)
# iremos obter a mesma média que através de trabalhar somente com as médias.

# Questão 4 Tem que copiar do livro o ponto 4.1.3 na mão

#Questão 5
# Para está igualdade ser verdadeira precisariamos classificar o Selection Bias
# e heteregoneus treatment effect bias como nulos, pra isso seria necessário 
# afirmar que o tratamento não possui nenhum efeito na média dos resultados,
# ou seja, ATT = ATU e que o grupo selecionado tem exatamente a mesma média de 
# resultados independente do tratamento, ou seja,{E[Y0|D = 1] ??? E[Y0|D = 0]} = 0


#Questão 6

Y1 <- matrix(c(8,9,8,4,7,1,5,7,5,4,5,10,5,10,2), ncol=1, nrow=15, byrow=F)
Y1
Y2 <- matrix(c(6,5,4,3,2,1,4,6,4,5,2,3,4,5,1), ncol=1, nrow=15, byrow=F)
Y2
D  <- matrix(c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0), ncol=1, nrow=15, byrow=F)
D

#item A:
ATE <- mean(Y1)-mean(Y2)
ATE

#item B: (D=1)
ATT1 <- matrix(c(9,4,1,7,4,10,10), ncol=1, nrow=7, byrow=F)
ATT2 <- matrix(c(5,3,1,6,5,3,5), ncol=1, nrow=7, byrow=F)
ATT <- mean(ATT1) - mean(ATT2)
ATT

#item C: (D=0)
ATU1 <- matrix(c(8,8,7,5,5,5,5,2), ncol=1, nrow=8, byrow=F)
ATU2 <- matrix(c(6,4,2,4,4,2,4,1), ncol=1, nrow=8, byrow=F)
ATU <- mean(ATU1) - mean(ATU2)
ATU

#Como já possuímos os estados factuais e contrafactuais, podemos calcular o ATE (item A).
#Assim, o medicamento oferece na média 2,333 anos de vida a mais que não tomá-lo

#Questão 7

# 20*0.65 + 40*0.25 + 60*.10 = 29

# 7 - B
# 20*0.10 + 40*0.25 + 60*0.65 = 51 
# Aumentou, mostrando que se os fumandores de cigarros tivessem a mesma distribuição
# de idade que os fumadores de pipe também teriam uma taxa de mortalidade maior

# Questão 8

library(tidyverse)
library(haven)

read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

training_example <- read_data("training_example.dta") %>% 
  slice(1:20)

ggplot(training_example, aes(x=age_treat)) +
  stat_bin(bins = 10, na.rm = TRUE)

ggplot(training_example, aes(x=age_control)) +
  geom_histogram(bins = 10, na.rm = TRUE)

#Resolução
earnings_trainees <- matrix(c(9500,12250,11000,11750,13250,10500,9750,10000,
                              10250,12500), ncol=1, nrow=10, byrow=F)
#Pegar dados matched por idade
earnings_non_trainees <- matrix(c(8050,10525,9400,10075,11425,8950,
                                  8275,8500,8725,9875), ncol=1, nrow=10, byrow=F)
et <-mean(earnings_trainees)
ent <-mean(earnings_non_trainees)
impacto_do_programa <- et-ent


# Após montar a tabela de matched sample é possível observar que os empregados com
# a mesma idade na empresa ganham em média 1695 a mais quando estão no grupo de "Trainees"
# em comparação ao grupo de "Non Trainees", mostrando que existe sim impacto positivo 
# do programa sobre o salário dos funcionários.

# Questão 9
# Regressão em discontinuidade é recomendado em casos que um determinado tratamento é 
# aplicado a partir de um limite "aleatorio", um bom exemplo seriam os alunos que completaram
# o ensino médio e fizeram a prova da fuvest, ao redor da nota de corte existe uma variação
# grande na probabilidade de matricular-se. Isso também vai gerar efeito na renda dos 
# participantes, aqueles que entraram na universidade terão maior probabilidade de matricular-se
# e posteriormente maior renda.

# Questão 10 - a
# Ter uma nota SAT acima da nota de corte influencia os salários dos aplicantes mesmo que
# sua diferença seja minima, como podemos observar se vermos o participante que tirou 1 ponto
# abaixo da nota de corte e o participante que tirou 1 ponto acima, estes participantes são
# muito próximos em caracteristicas observáveis, porém como a regra do tratamento (universidades)
# é aplicada a partir de um determinado ponto, podemos ver q a partir da nota de corte os
# saláros sobem em média 9.5%, provando que o efeito de entrar na universidade impacta
# renda.

#Questão 10 - b
# Mostra que a discontinuidade é de 9.5% na nota de corte, com uma probabilidade de 
# 99.869%, devido as estimativas de bandwith que foram usadas no modelo (7.4% a 11.1%)

# Questão 11
# 