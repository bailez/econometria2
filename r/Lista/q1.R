
# 1. Use GPA3.RAW for this exercise. The data set is for 366 student-athletes from a
# large university for fall and spring semesters. [A similar analysis is in Maloney
# and McCormick (1993), but here we use a true panel data set.] Because you have
# two terms of data for each student, an unobserved effects model is appropriate.
# The primary question of interest is this: Do athletes perform more poorly in school
# during the semester their sport is in season?
# 
# (a) Use pooled OLS to estimate a model with term GPA (trmgpa) as the dependent
# variable. The explanatory variables are spring, sat, hsperc, female, black, white,
# frstsem, tothrs, crsgpa, and season. Interpret the coefficient on season. Is it
# statistically significant?

library("plm")
library("wooldridge")
data("gpa3")

gpa3_data <- pdata.frame(gpa3, index = dim(gpa3)[1])
reg <- plm(trmgpa ~ spring + sat + hsperc +  female + black + white + frstsem + 
             tothrs + crsgpa + season, data = gpa3_data, model ="pooling" )

summary(reg)

#Notar essa observação
print("Testes de Coeficientes")
coeftest(reg)

print("Teste Robusto")
coeftest(reg, vcov. = vcovHC)

# The season coeficient is not statistically significant. It's p-value is very big
# (0.57228) in both robust and regular coeficient tests

