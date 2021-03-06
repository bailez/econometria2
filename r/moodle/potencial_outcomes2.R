library(tidyverse)

po <- tibble(
  patients = c(1:10),
  y1 = c(7,5,5,7,4,10,1,5,3,9),
  y0 = c(1,6,1,8,2,1,10,6,7,8))

po <- po %>%
  mutate(sigma = po$y1-po$y0)

#ATE � a diferen�a m�dia entre as colunas 1 e 3 ou � a m�dia dos efeitos heterog�neos de tratamento
ATE <- mean(po$y1) - mean(po$y0)

# Agora vamos supor que existe um m�dico perfeito que escolhe o melhor tratamento
#dados os resultados potenciais de cada indiv�duo (ou seja, eu estou colocando vi�s de sele-
# ��o e, .:, violando independ�ncia de Y1, Y0 em rela��o a D).

po <- po %>%
  mutate(D = c(1, 0, 1, 0, 1, 1, 0, 0, 0, 1))

po <- po %>%
  mutate(Y = ifelse(po$y1 > po$y0, po$y1, po$y0))

ATT <- po %>%
  filter(D == 1) 

ATT<- mean(ATT$y1) - mean(ATT$y0) # a m�dia de vida p�s cirurdia para o grupo tratamento �
# 4.4

ATU <- po %>%
  filter(D == 0)

ATU <- mean(ATU$y1) - mean(ATU$y0) # a m�dia de vida p�s cirurdia para o grupo controle �
# -3.2

# ATE � uma m�dia ponderada entre ATT e ATU. Observe que o efeito tratamento tem efeitos
#negativos para alguns pacientes (este � o efeito heterog�neo)

# Estimador : simple diferrence in means (SDO)

mean_d1 <- po %>% filter(D == 1)
mean_d1 <- mean(mean_d1$Y)
mean_d0 <- po %>% filter(D == 0)
mean_d0 <- mean(mean_d0$Y)

SDO <- mean_d1 - mean_d0 # o grupo de tratamento vive 0.4 menos p�s cirurgia do que o grupo de controle.
#SDO deu diferente do ATE... por que?
# O tratamento n�o foi atribuido aleatoriamente! Ou seja, os resultados potenciais n�o s�o
# idenpendentes do tratamento... temos efeito heterog�neo e vi�s de sele��o.

# Calculando o vi�s de sele��o

mean_d1y0 <- po %>% filter(D == 1)
mean_d1y0 <- mean(mean_d1y0$y0)
mean_d0y0<- po %>% filter(D == 0)
mean_d0y0 <- mean(mean_d0y0$y0)
vies <- mean_d1y0 - mean_d0y0

# Calculando o efeito heterog�neo

ef_heterogeneo <- (1 - 0.5)*(ATT - ATU)


# No mundo real, n�o temos os resultados potenciais. Para evitar vi�s de sele��o, precisamos
#randomizar o tratamento aleatorio.

gap < function() {
  
po <- po %>%
  mutate(random = rnorm(10)) %>%
  arrange(random) %>%
  mutate(D2 = c(rep(1, 5), rep(0, 5)))

mean2_d1 <- po %>% filter(D2 == 1)
mean2_d1 <- mean(mean2_d1$Y)
mean2_d0 <- po %>% filter(D2 == 0)
mean2_d0 <- mean(mean2_d0$Y)
sdo <- mean2_d1 - mean2_d0

return(sdo)

}

sim <- replicate(10000, gap())
meansim <-mean(sim) # SDO deu muito pr�ximo de ATE! Eliminamos vi�s e efeito heterog�neo. 
