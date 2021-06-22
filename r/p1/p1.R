#GABARITO DA PROVA DE ECONOMETRIA 2 DA  TURMA 2

library(plm)
library (wooldridge)
library(tidyverse)
library(stargazer)
library(magrittr) 
library(haven)
library(lmtest)

#------------------------------------ primeira questão -----------------------------------------

#Questão 1
##Montando a Matriz
y <- matrix(data = c(3,14,20,22,23,33), nrow = 6, ncol = 1 )
y

#Matriz X
x <- matrix(data = c(1,1,1,1,1,1,0,2,2,4,4,6,0,4,8,4,8,10), nrow = 6,ncol = 3)
x

#Matriz Transposta 
xt <- t(x)
xt

#a) Matriz Inversa
xtx <- xt%*%x
x_inv <- solve(xtx)
print(x_inv)

#Calculando os betas - beta = (X'X)^(-1)X'Y
betas <- x_inv%*%xt%*%y
print(betas)

#b)
# y = 3.688073 +  3.114679*X2i + 1.082569*X3i
# beta1 = 3.688073
# beta2 = 3.114679
# beta3 = 1.082569

#Calulando a variância estimada total
##Sigma2 = SSR/n-k
##SSR = Y'Y-betachapeu'X'Y
##Matriz Variância-Covariância = sigma2(X'x)^(-1)

yty <- t(y)%*%y
yty

betaT <- t(betas)
betaT

xty <- xt%*%y 
xty

ssr <- yty-betaT%*%xty
ssr

nobsv <- nrow(x)
nobsv

k <- ncol(x)
k

sigma2 <- ssr/(nobsv - k)
sigma2

msigma2 <- matrix(sigma2, ncol = 3, nrow = 3, byrow = TRUE )

varcov <- msigma2*x_inv
varcov

#Erro Padrão dos betas
ep <- sqrt(diag(varcov))
ep

ep_beta1 <- ep[1]
ep_beta2 <- ep[2]
ep_beta3 <- ep[3]

cbind(ep_beta1,ep_beta2,ep_beta3)

#obtendo o teste de significância
##tabelados de t para 5%
gl <- (nrow(x) -k)

qt(0.025,gl)

tabelado5 <- abs(qt(0.025,gl))

tabelado5

tbeta2 <- ((betas[2] - 0)/ep_beta2)
print(tbeta2)

#c) Como o tbeta2 é maior do que o nosso tabelado5, podemos recusar a hipotese nula com 95% de confiança.

ic_inferior3 <- betas[3] - ep_beta3*tabelado5
ic_superior3 <- betas[3] + ep_beta3*tabelado5
cbind(ic_inferior3,ic_superior3)

# Alunos que  corrigiram para k-graus de liberdade ganharam 100 % da questão . Os que não fizeram ganharam 80%.
# Tinha que lembrar de usar o comando as.numeric. Expliquei isso em monitoria. Alguns codes não rodaram por isso e tbm
#tiveram descontos em sua nota. Ainda assim, codes incompletos, ou que rodaram parcialmente ganharam pontos parciais. 

#alguns alunos erraram o intervalo de confiança, mas ainda assim ganharam questão incompleta.




#d) O intervalo de confiança é afetado diretamente pelo graus de liberdade que acaba afetando o valor tabelado e também o erro
#padrão dos estimadores

##Resumindo...idêntico ao que fizemos em monitoria.
#O resultado é confiável? Não! 
# A base de dados possui poucas observações, pouca variabilidade e poucas informações.
# Isso era necessário para explicar os resultados
#alunos que explicaram com detalhes ganharam ponto cheio.
######################################

#e
#RQuadrado = 1 - SSR/SST
## SST = Y'Y - nymean^2

ymean <- mean(y)
ymean

ysquared <- nobsv*ymean^2
ysquared

sst <- yty - ysquared
sst

r2 <- 1 - (ssr/sst)
r2

#O Rquadrado que chegamos significa que aproximadamente 98% do efeito causado na variável dependente pode ser explicar 
# pode ser explicado pelas variáveis explicativas. 

#A questão pede para explciar a questão do R2. Por qual motivo o r2 é alto? Isso é confiável?

# como a base de dados é pequena, o r2 não é confiável. Como economista, vc deve chegar a conclusão que 
# r2 aqui significa quase nada. Contudo, quem explicou de maneira satisfatória ganhou entre 80 a 100% da questão.
# A maioria ganhou 100%. Contudo, alunos que erraram code por algum motivo, ganharam entre 20 a 60%.

#Resposta de outra aluna:

#Como R2 está perto de 1, o modelo consegue ser explicado pelas variáveis elencadas.
#Entretanto, por se tratar de uma amostra pequena, o R2 só nos diz que 98% da regressão
#pode ser explicada por X2 e X3. O R2 não é uma medida que deve ser utilizada indiscriminadamente 






#--------------------------------------------------------------------------------------------------------------


###################################
#Resposta de uma aluna da segunda questão de  uma aluna que tirou 10
#Questão2 - "A"
regressao <- lm(lwage ~ married + educ + hours, data=wagepan)
print(regressao)
summary(regressao)
coeftest(regressao)
bregressao <- coef(regressao)
bregressao
# Aqui rodei uma regressão simples usando lm. As variáveis significativas são se o 
#individuo é casado ou não e anos de educação. 1 ano adicional em eduação impacta o 
#salário em 7,5%,aproximadamente, e indivíduos casados tendem também a terem salários 
#maiores. O R2 é baixo, e o R2 ajustado também, apenas 10,5% dessa regressão se explica
#pelas variáveis levantadas. 

#Questão 2, "B"
regressaoplm <- plm(lwage ~ married + factor(year) + factor(year)*educ + educ+hours, data = wagepan, index=c("year"), model="pooling")
summary(regressaoplm)

#Aqui, observamos que "married", assim como "educ" e "hours" são variáveis significativas.  
#Quando fixamos os efeitos, a educação passa a ter um efeito de 5,8% nos salários, e indivíduos 
#casados também têm maiores salários.O R^2 aumentou, o que dá uma força maior ao modelo, 
#se comparado com a regressão ols. 

#Questão 2, "C"
coeftest(regressaoplm, vcov.=vcovHC)
summary(regressaoplm, vcov.=vcovHC)
#Quando há problemas de heterocedasticidade, a estatística t não segue uma distribuição normal, 
#mesmo em amostras grandes.Isso pode invalidar a inferência ao testar a hipótese.
#Ao rodar a regressão robusta,garantimos que não rejeitamos uma hipótese sem querer, 
#mesmo que ela seja verdadeira. Os erros padrão diminuem, comparado ao modelo 
#sem robustez. 


#################################################################33




#-----------------------------------------------------------------------------------------------

#################### TERCEIRA QUESTÃO #########################################



# PRimeira possibilidade de resolução. Uma aluna respondeu de maneira perfeita esta questão. Ou seja,
# a devida aluna prestou atenção nas dicas dadas em monitoria
# e estava 100% preparada para responder esse item. 
#Além disso, o aspecto teórico abordado pelo professor também é contemplado nessa questão.


###################################################################### 3A - ATE

dados <- tibble(
  patients = c(1:15),
  y1 = c(10,12,8,5,7,4,5,11,5,4,9,10,4,13,5),
  y0 = c(6,4,5,4,3,2,6,4,3,8,6,5,3,8,1))

# Criando meu sigma, que é o ganho de estado de ter recebido o tratamento
dados <- dados %>%
  mutate(sigma = dados$y1-dados$y0)

#ATE é a diferença média entre as colunas 1 e 3, isto é, a média dos efeitos heterogêneos de tratamento
ATE <- mean(dados$y1) - mean(dados$y0)
print(ATE) # 3.133333, ou seja, overall o tratamento é positivo

###################################################################### 3B - ATT

# Discriminando, manualmente, se recebeu ou não o tratamento
dados <- dados %>%
  mutate(D = c(0, 1,0, 1, 0, 1, 0, 1, 0, 1,0, 1, 0, 1,0))

dados <- dados %>%
  mutate(Y = ifelse(dados$y1 > dados$y0, dados$y1, dados$y0))
View(dados)

# Primeiro filtrando apenas quem recebeu o tratamento (D==1)
ATT <- dados %>%
  filter(D == 1) 

# Tirando a média dos efeitos
ATT<- mean(ATT$y1) - mean(ATT$y0) # Média do ganho de estado de quem recebeu o tratamento

print(ATT) # 


###################################################################### 3B - ATU

# Agora filtrando apenas quem não recebeu o tratamento (D==0)
ATU <- dados %>%
  filter(D == 0)

# Tirando a média dos efeitos
ATU <- mean(ATU$y1) - mean(ATU$y0) # Média do ganho de estado de quem NÃO recebeu o tratamento
print(ATU) #  



###################################################################### 3C - É eficiente?

# Neste exercício eu CONHEÇO os contrafactuais e consigo opinar sobre ATE, que é positivo, portanto o tratamento é benéfico
# Na vida real, eu não conheceria os contrafactuais. Com isso, teria que fazer a estimativa amostral dos efeitos, isto é, inferir apenas sobre o efeito médio AMOSTRAL 
# Seria assim:

# Indicador chamado Simple Diferrence in Means (SDO)

# Filtrando quem recebeu o tratamento
mean_d1 <- dados %>% filter(D == 1)
# Tirando a média dos ESTADOS, não dos ganhos (pois estes eu não observo)
mean_d1 <- mean(mean_d1$Y)
# Filtrando quem não recebeu o tratamento
mean_d0 <- dados %>% filter(D == 0)
# Tirando a média dos ESTADOS, não dos ganhos (pois estes eu não observo)
mean_d0 <- mean(mean_d0$Y)

SDO <- mean_d1 - mean_d0 # 

print(SDO)


##################


# Questão de um aluno que tirou 10_______________________________

#-----------------------------------------------------------------------------
#Questão 3
#Montando a Tabela de Expectativa de Vida

y1 <- c(10,12,8,5,7,4,5,11,5,4,9,10,4,13,5)
y2 <- c(6,4,5,4,3,2,6,4,3,8,6,5,3,8,1)
d <- c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0)

total <- c(y1,y2,d)
total 

tabela <- matrix(data = total, ncol = 3, byrow = F)
tabela

t11 <- c(y1[2],y1[4],y1[6],y1[8],y1[10],y1[12],y1[14])
t10 <- c(y2[2],y2[4],y2[6],y2[8],y2[10],y2[12],y2[14])

tratado <- matrix(data = c(t11,t10), ncol = 2, byrow = F)
tratado

t01 <- c(y1[1],y1[3],y1[5],y1[7],y1[9],y1[11],y1[13],y1[15])
t00 <- c(y2[1],y2[3],y2[5],y2[7],y2[9],y2[11],y2[13],y2[15])

controle <- matrix(data = c(t01,t00), ncol = 2, byrow = F)
controle

#Efeito Médio do Grupo Tratado
ATT <- mean(tratado[,1]) - mean(tratado[,2])
ATT

#Efeito Médio do Grupo Controle 
ATU <- mean(controle[,1]) - mean(controle[,2])
ATU

#Efeito Tratamento
pi <- nrow(tratado)/nrow(tabela)
pi

ATE <- pi*ATT + (1-pi)*ATU

#Efeito Tratamento,Efeito Médio do Grupo Tratado e Efeito Médio do Grupo Controle 
q3 <- cbind(ATE, ATT, ATU)
q3

#d) Podemos perceber que o efeito médio do tratamento é positivo, havendo um uma média de 2.93 anos de expectativa de vida.
## quando comparamos também o ATT e o ATU podemos verificar que o efeito médio em receber o tratamento gera uma expectativa
## de vida maior do que apenas receber o placebo. Evidenciando que o tratamento é sim eficaz. 

#-----------------------------------------------------------------------------


















#-------------------------QUESTÃO 4---------------------------

###################################################################### ESsa questão foi mais uma questão 
# gabarito de uma aluna. A mesma desenvolveu todos os pontos analisados em aula.
#Além disso, abordou com precisão o que foi pedido.


read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}



# Criando a variável binária d (tratamento), que significa ser (1) ou não (0) da primeira classe
titanic <- read_data("titanic.dta") %>% 
  mutate(d = case_when(class == 1 ~ 1, TRUE ~ 0))
View(titanic)

######### Modelo simples, assumindo H1 (independência) e H2 (suporte comum) 

##### E(Y1) 
# Filtro apenas os que receberam o tratamento (D=1), que aqui significa primeira classe
ey1 <- titanic %>% 
  filter(d == 1) %>%
  pull(survived) %>% 
  mean()

#### E(Y0)
# Filtro apenas os que NÃO receberam o tratamento (D=0), que aqui significa pertencer a classe inferior
ey0 <- titanic %>% 
  filter(d == 0) %>%
  pull(survived) %>% 
  mean()


####### SDO = E(Y1/D=1) - E(Y0/D=0)     Tudo isso aqui é amostral
SDO <- ey1 - ey0

print(SDO)

# Estar na primeira classe significa uma taxa de sobrevivencia 35.4% maior segundo os dados amostrais. 
#Note que aqui não foi realizado nenhum procedimento de subclassificação. O aluno deveria perceber
# que ao realizar esse procedimento poderia ser induzido ao erro.
# AInda assim, questões minimamente bem explicadas ganharam 100% da pontuação. Questões com falhas 
# na explicação ganharam 50 a 80 % .
#Perceba então  que mulheres e crianças são associadas a grupos de prioridade.



################################################################################# 4. B

#### Testando se idade e gênero atuaram como backdoors (covariadas desequilibradas)


titanic %<>%
  mutate(s = case_when(sex == 0 & age == 1 ~ 1, # homens jovens
                       sex == 0 & age == 0 ~ 2, # homens velhos
                       sex == 1 & age == 1 ~ 3, # mulheres jovens
                       sex == 1 & age == 0 ~ 4, # mulheres velhas
                       TRUE ~ 0))

# homens, jovens, primeira-classe
ey11 <- titanic %>% 
  filter(s == 1 & d == 1) %$%
  mean(survived)

# homens, jovens, outras classes
ey10 <- titanic %>% 
  filter(s == 1 & d == 0) %$%
  mean(survived)

# homens, adultos, primeira-classe
ey21 <- titanic %>% 
  filter(s == 2 & d == 1) %$%
  mean(survived)

# homens, adultos, outras classes
ey20 <- titanic %>% 
  filter(s == 2 & d == 0) %$%
  mean(survived)

# mulheres, jovens, primeira-classe
ey31 <- titanic %>% 
  filter(s == 3 & d == 1) %$%
  mean(survived)

# mulheres, jovens, outras classes
ey30 <- titanic %>% 
  filter(s == 3 & d == 0) %$%
  mean(survived)

# mulheres, adultas, primeira-classe
ey41 <- titanic %>% 
  filter(s == 4 & d == 1) %$%
  mean(survived)

# mulheres, adultas, outras classes
ey40 <- titanic %>% 
  filter(s == 4 & d == 0) %$%
  mean(survived)

# Diferenças nas médias de cada grupo primeira-classe com o outras classes
diff1 = ey11 - ey10 # homens jovens
diff2 = ey21 - ey20 # homens velhos
diff3 = ey31 - ey30 # mulheres jovens
diff4 = ey41 - ey40 # mulheres velhas

# Número de observações para cálculo dos pesos relativos
obs = nrow(titanic %>% filter(d == 0))

# Número de homens jovens em outras classes
wt1 <- titanic %>% 
  filter(s == 1 & d == 0) %$%
  nrow(.)/obs

# Número de homens adultas em outras classes
wt2 <- titanic %>% 
  filter(s == 2 & d == 0) %$%
  nrow(.)/obs

# Número de mulheres jovens em outras classes
wt3 <- titanic %>% 
  filter(s == 3 & d == 0) %$%
  nrow(.)/obs

# Número de mulheres adultos em outras classes
wt4 <- titanic %>% 
  filter(s == 4 & d == 0) %$%
  nrow(.)/obs

# Criando o ATE ponderado e estratificado
wATE = diff1*wt1 + diff2*wt2 + diff3*wt3 + diff4*wt4

stargazer(wATE, SDO, type = "text")



# RESPOSTA: Depende. Os sinais do SDO e do wATE apontam para o mesmo sentido (estar na primeira classe aumentava as chances de sobrevivência)
# Porém o SDO SUBESTIMAVA a magnitude deste efeito (18.9% frente wAGE de 35.4%)
#
#Resposta também  completa:
#Calculando o SDO chegamos a conclusão que estar sentado na primeira classe aumetaria a chance de sobreviência em
#aproximadamente 35%. Porém, podemos perceber que nessa base de dados nos não ajustamos as confounders observáveis de idade
#genêro (subclassification), portanto podemos dizer que essa é uma estimativa enviesada de ATE.

#Essa questão foi a que mais a turma obteve acertos.


### Resolução alternativa da questão 4 de outra aluna que tirou 10############

#Questão 4, Titanic. 
library(tidyverse)
library(haven)
read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

titanic <- read_data("titanic.dta") %>% 
  mutate(d = case_when(class == 1 ~ 1, TRUE ~ 0))

#Média dos passageiros da 1ª classe que sobreviveram 
ey1<-titanic %>% 
  filter(d == 1) %>%
  pull(survived) %>% 
  mean()

#Média dos passageiros que não sobreviveram
ey0 <- titanic %>% 
  filter(d == 0) %>%
  pull(survived) %>% 
  mean()

#Diferença simples e ingênua nos resultados (SDO):
sdotitanic <- ey1 - ey0
sdotitanic

#Mostra que estar na primeira classe aumenta a probabilidade de sobreviver em 35,4%. 
#O procedimento não gera resultado semelhante ao ATE ponderado, já que, se as mulheres
#e crianças estivessem mais concentradas na primeira classe, poderia enviesar nossas
#estimativas, contaminando nossa hipótese de que primeira classe tem mais chance de 
#sobreviver. 

#Com subclassificação
library(stargazer)
library(magrittr) # for %$% pipes
library(tidyverse)
library(haven)

titanic <- read_data("titanic.dta") %>% 
  mutate(d = case_when(class == 1 ~ 1, TRUE ~ 0))

#Agora eu vou criar colunas para cada estrato:
#s=1 quando for homem e criança
#s=2 quando for homem e adulto
#s=3 quando for mulher e criança
#s=4 quando for mulher e adulta

titanic %<>%
  mutate(s = case_when(sex == 0 & age == 1 ~ 1,
                       sex == 0 & age == 0 ~ 2,
                       sex == 1 & age == 1 ~ 3,
                       sex == 1 & age == 0 ~ 4,
                       TRUE ~ 0))

#Criança homem que sobreviveu e estava na primeira classe. 
ey11 <- titanic %>% 
  filter(s == 1 & d == 1) %$%
  mean(survived)

#Criança homem que não estava na primeira classe
ey10 <- titanic %>% 
  filter(s == 1 & d == 0) %$%
  mean(survived)

#Homem adulto que estava na primeira classe 
ey21 <- titanic %>% 
  filter(s == 2 & d == 1) %$%
  mean(survived)

#Homem adulto que não estava na primeira classe
ey20 <- titanic %>% 
  filter(s == 2 & d == 0) %$%
  mean(survived)

#Mulher criança que estava na primeira classe 
ey31 <- titanic %>% 
  filter(s == 3 & d == 1) %$%
  mean(survived)

#Mulher criança que não estava na primeira classe
ey30 <- titanic %>% 
  filter(s == 3 & d == 0) %$%
  mean(survived)

#Mulher adulta que estava na primeira classe
ey41 <- titanic %>% 
  filter(s == 4 & d == 1) %$%
  mean(survived)

#mulher adulta que não estava na primeira classe 
ey40 <- titanic %>% 
  filter(s == 4 & d == 0) %$%
  mean(survived)


diff1 = ey11 - ey10
diff1
diff2 = ey21 - ey20
diff2
diff3 = ey31 - ey30
diff3
diff4 = ey41 - ey40
diff4

obs = nrow(titanic %>% filter(d == 0))
obs

#Strata-specific weights

wt1 <- titanic %>% 
  filter(s == 1 & d == 0) %$%
  nrow(.)/obs
wt1

wt2 <- titanic %>% 
  filter(s == 2 & d == 0) %$%
  nrow(.)/obs
wt2

wt3 <- titanic %>% 
  filter(s == 3 & d == 0) %$%
  nrow(.)/obs
wt3

wt4 <- titanic %>% 
  filter(s == 4 & d == 0) %$%
  nrow(.)/obs
wt4

#Weighted average survival rate
wate = diff1*wt1 + diff2*wt2 + diff3*wt3 + diff4*wt4
wate

stargazer(wate, sdotitanic, type = "text")

#Com pesos, a probabilidade de sobreviver estando na primeira classe é de 18,9%, e 
#não mais de 35,4%, como estimamos inicialmente. 



