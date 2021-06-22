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


# Questão 1 da lista

library("wooldridge")
data("gpa3")
#panel OLS : significa rodar todo mundo sem verificar efeito tempo ou efeito de grupo. 

gpa3.p <- pdata.frame(gpa3, index = 732)
print(gpa3.p)
regressao_gpa3<- (plm(trmgpa~ spring + sat +hsperc+  female + black + white + frstsem+
              tothrs + crsgpa + season, data=gpa3.p, model ="pooling" ))

summary(regressao_gpa3)
#Notar essa observação
coeftest(regressao_gpa3)
coeftest(regressao_gpa3, vcov. = vcovHC)


#############################QUESTÃO 2###########################################################################################3


data("wage2")
#1ST STAGE
stage1<- lm(educ~ sibs + exper + tenure + black, data = wage2)
summary(stage1)

stage2<-lm( log(wage) ~ fitted(stage1)+ exper + tenure + black, data = wage2)
summary(stage2)

# Agora 2sls automático
aut.2sls <- ivreg( log(wage) ~ educ+ exper + tenure + black|sibs+ exper + tenure + black, data = wage2)

stargazer(stage2, aut.2sls, type = "text", keep.stat = c("n", "rsq"))


#################******************* Dados em painel com efeitos fixos************ ####################################################
# código extra

data(wagepan)
wagepan.p <- pdata.frame(wagepan, index = c("nr", "year"))
pdim(wagepan.p)

efeitofixo<- plm(lwage~ married+ union + factor(year)*educ, data = wagepan.p, model = "within")
summary(efeitofixo)
#comparando com pooled
pooled<- plm(lwage~ married+ union + factor(year)*educ, data = wagepan.p, model = "pooling")
summary(pooled)


######################## questão 4 ##############################################

data("jtrain")
pdim(jtrain)
jtrain.p <- pdata.frame(jtrain)
estimate<- lm(hrsemp ~ d88 + d89 + grant + grant_1 + log(employ), data = jtrain.p)
summary(estimate)

jtrain.p <- pdata.frame(jtrain)
estimate_fd<- plm(hrsemp ~  grant + grant_1 + log(employ), data = jtrain.p, model = "fd")
summary(estimate_fd)



###################################Questão 5##################################################
#aggregating the data

library(tidyverse)
library(haven)
library(estimatr)

read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

lmb_data <- read_data("lmb-data.dta")

categories <- lmb_data$lagdemvoteshare

demmeans <- split(lmb_data$score, cut(lmb_data$lagdemvoteshare, 100)) %>% 
  lapply(mean) %>% 
  unlist()

agg_lmb_data <- data.frame(score = demmeans, lagdemvoteshare = seq(0.01,1, by = 0.01))

#plotting
lmb_data <- lmb_data %>% 
  mutate(gg_group = case_when(lagdemvoteshare > 0.5 ~ 1, TRUE ~ 0))

ggplot(lmb_data, aes(lagdemvoteshare, score)) +
  geom_point(aes(x = lagdemvoteshare, y = score), data = agg_lmb_data) +
  stat_smooth(aes(lagdemvoteshare, score, group = gg_group), method = "lm", 
              formula = y ~ x + I(x^2)) +
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5)



########################QUestão 6 #############################


# Getting sample data.

library(foreign)
mydata = read.dta("http://dss.princeton.edu/training/Panel101.dta")

# Create a dummy variable to indicate the time when the treatment started. Lets
#assume that treatment started in 1994. In this case, years before 1994 will have a
#value of 0 and 1994+ a 1. If you already have this skip this step.
mydata$time = ifelse(mydata$year >= 1994, 1, 0)
# Create a dummy variable to identify the group exposed to the treatment. In this
#example lets assumed that countries with code 5,6, and 7 were treated (=1).
#Countries 1-4 were not treated (=0). If you already have this skip this step.
mydata$treated = ifelse(mydata$country == "E" |
                          mydata$country == "F" |
                          mydata$country == "G", 1, 0)
# Create an interaction between time and treated. We will call this interaction
#'did'.
mydata$did = mydata$time * mydata$treated
# Estimating the DID estimator
didreg = lm(y ~ treated + time + did, data = mydata)
summary(didreg)


# The coefficient for 'did' is the differences-in-differences
#estimator. The effect is significant at 10% with the treatment having
#a negative effect.


# Estimating the DID estimator (using the multiplication method, no
#need to generate the interaction)
didreg1 = lm(y ~ treated*time, data = mydata)
summary(didreg1)

# The coefficient for 'treated#time' is the differences-indifferences estimator ('did' in the previous example). The effect is
#significant at 10% with the treatment having a negative effect.



#### Exemplo  do livro de DIF IN DIF################

library(tidyverse)
library(haven)
library(estimatr)

read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

abortion <- read_data("abortion.dta") %>% 
  mutate(
    repeal  = as_factor(repeal),
    year    = as_factor(year),
    fip     = as_factor(fip),
    fa      = as_factor(fa),
    younger = as_factor(younger),
    yr      = as_factor(case_when(repeal == 1 & younger == 1 ~ 1, TRUE ~ 0)),
    wm      = as_factor(case_when(wht == 1 & male == 1 ~ 1, TRUE ~ 0)),
    wf      = as_factor(case_when(wht == 1 & male == 0 ~ 1, TRUE ~ 0)),
    bm      = as_factor(case_when(wht == 0 & male == 1 ~ 1, TRUE ~ 0)),
    bf      = as_factor(case_when(wht == 0 & male == 0 ~ 1, TRUE ~ 0))
  ) %>% 
  filter(bf == 1 & (age == 15 | age == 25))

regddd <- lm_robust(lnr ~ repeal*year + younger*repeal + younger*year + yr*year + fip*t + acc + ir + pi + alcohol + crack + poverty + income + ur,
                    data = abortion, weights = totpop, clusters = fip)

abortion_plot <- tibble(
  sd = regddd$std.error[110:124],
  mean = regddd$coefficients[110:124],
  year = c(1986:2000))

abortion_plot %>% 
  ggplot(aes(x = year, y = mean)) + 
  geom_rect(aes(xmin=1986, xmax=1992, ymin=-Inf, ymax=Inf), fill = "cyan", alpha = 0.01)+
  geom_point()+
  geom_text(aes(label = year), hjust=-0.002, vjust = -0.03)+
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = mean-sd*1.96, ymax = mean+sd*1.96), width = 0.2,
                position = position_dodge(0.05))


############ QUESTÃO 7 #############################
# Load rddtools package
library("rddtools")
# Use data from Lee (2008)
data(house)
# Set outcome, forcing and cutoff variable
plot(house)
house_rdd <- rdd_data(y=y, x=x, cutpoint= 0, data=house)
plot(house_rdd)
# Estimate RDD
reg_para <- rdd_reg_lm(rdd_object=house_rdd)
# Print results
print(reg_para)

plot(reg_para)


# Restrict sample to bandwidth area
bw_ik <- rdd_bw_ik(house_rdd)
reg_para_ik <- rdd_reg_lm(rdd_object=house_rdd, bw=bw_ik)
reg_para_ik

# Load rdrobust package
library("rdrobust")
# Estimate effect
rdrobust(house$y, house$x)
rdplot(house$y, house$x)

#############################################################################



