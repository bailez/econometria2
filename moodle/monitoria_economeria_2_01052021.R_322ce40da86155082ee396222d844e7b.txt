
#Comentários iniciais.
#Como prometido, fiz um mini roteiro. Da primeira parte, repliquem os exercícios matriciais e façam a 
#estimaçao de beta, da matriz de variância e covariâncias e de intervalo de confiança. Esta é a parte 1

#Parte 2: Repliquem este código. Os gráficos são para entender e explicarei na próxima aula.

library(foreign)
Panel <- read.dta("http://dss.princeton.edu/training/Panel101.dta")
coplot(y ~ year|country, type="l", data=Panel) # Lines
coplot(y ~ year|country, type="b", data=Panel)# Points and lines

#gráfico para apreciar, não cai em prova
library(car)
scatterplot(y~year|country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=Panel)

#gráfico para entender. Não cai em prova( mas pode ser um bônus!)
library(gplots)
plotmeans(y ~ country, main="Heterogeineity across countries", data=Panel)
plotmeans(y ~ year, main="Heterogeineity across years", data=Panel)


#rodar OLS -> mesma coisa que o plm pooled igual em aula.
# vocês não viram ainda painel ainda. Vamos ficar apenas no comecinho.

#panel OLS : significa rodar todo mundo sem verificar efeito tempo ou efeito de grupo. Pergunta de prova:
# Esse procedimento é correto? Quais são suas limitações. Reflita.

Panel <- read.dta("http://dss.princeton.edu/training/Panel101.dta")
ols <-lm(y ~ x1, data=Panel)
summary(ols)


#OLS com efeito fixo. Reflita. É correto?

fixed.dum <-lm(y ~ x1 + factor(country) + factor(year) - 1, data=Panel)
summary(fixed.dum)

yhat <- fixed.dum$fitted
#gráfico para apreciar. Não cai em prova.
library(car)
scatterplot(yhat~Panel$x1|Panel$country, boxplots=FALSE, xlab="x1", ylab="yhat",smooth=FALSE)
abline(lm(Panel$y~Panel$x1),lwd=3, col="red")

install.packages("apsrtable")
#Pacote novo. Pode dar bônus na prova de vocês
library(apsrtable)
apsrtable(ols,fixed.dum, model.names = c("OLS", "OLS_DUM"))

####################################
#Agora vamos ao procedimento "correto"
library(plm)
fixed <- plm(y ~ x1, data=Panel, index=c("country", "year"), model="within")

summary(fixed)
fixed_year <- plm(y ~ x1 + factor(year), data=Panel, index=c("country", "year"), model="within")
summary(fixed_year)

fixed_year_country <- plm(y ~ x1 + factor(year)+ factor(country), data=Panel, index=c("country", "year"), model="within")
summary(fixed_year_country)
fixef(fixed_year_country) #Display the fixed effects (constants for each country(incluso year. Vou explicar em aula))




#Aqui eu provo a vcs que fixed é melhor que ols.
fixef(fixed) # Display the fixed effects (constants for each country)
pFtest(fixed, ols)


#erros padrão do teste t
library(sandwich)
library(lmtest)
coeftest(ols)
coeftest(fixed)
coeftest(fixed, vcov. = vcovHC)


#Parte 3. Estou elaborando.
# Ao todo serão 3 questões na prova prática.

#obs: Puxando efeito Year e country para ver o X1
fixed_year_country[["model"]]



