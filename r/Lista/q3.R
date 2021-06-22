# 3. Use the data in HTV.RAW for this exercise:
# 
#   (A) Run a simple OLS regression of log(wage) on educ. Without controlling for 
# other factors, what is the 95% confidence interval for the return to another year
# of education?
#   
#   (B) Now, add to the simple regression model in part (a) a quadratic in experience
# and a full set of regional dummy variables for current residence and residence
# at age 18. Also include the urban indicators for current and age 18 residences.
# What is the estimated return to a year of education?
#   
#   (C) Estimate the model from part (b) by IV, using ctuit as an IV for educ. How does
# the confidence interval for the return to education compare with the OLS CI from
# part (b)?

library("wooldridge") #importar htv
library("AER") #usar ivreg

data("htv")

## (A) ##

reg <- lm(log(wage) ~ educ, data =htv)
summary(reg)
ci <- confint(reg, 'educ', level=0.95)
print(ci)

## (B) ##
  
reg <- lm(log(wage) ~ educ + 
            exper^2 + # quadratic experience
            west + south + ne + nc + # variaveis regionais
            west18 + south18 + ne18 + nc18 + # regionais se 18 anos
            urban + urban18, # urban indicators
          data =htv)
summary(reg)
ci <- confint(reg, 'educ', level=0.95)
print(ci)

# o retorno estimado 1 ano de educação é de um aumento de 13.4477% da renda

## (C) ##

two_stages <- ivreg(log(wage) ~ educ + exper^2 + west + south + ne + nc + west18 + 
                   south18 + ne18 + nc18 + urban + urban18 |                            
                              ctuit + exper^2 + west + south + ne + nc + west18 +
                   south18 + ne18 + nc18 + urban + urban18,data =htv)

summary(two_stages)
ci_iv <- confint(two_stages, 'educ', level=0.95)
print(ci_iv)

# O intervalo de confiança com a variavel instrumental é maior que o da ols ,
# e não cobre o valor 0. A significancia estatística aumentou na IV