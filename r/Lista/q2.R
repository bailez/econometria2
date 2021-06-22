# 2. The purpose of this exercise is to compare the estimates and standard errors 
# obtained by correctly using 2SLS with those obtained using inappropriate procedures.
# 
# Use the data file WAGE2.RAW.
# 
#   (A) Use a 2SLS routine to estimate the equation.
#   
#   log(wage) = β0 + β1educ + β2exper + β3ternure + β4black + u (1)
# 
# where sibs is the IV for educ. Report the results in the usual form.

library("wooldridge") # importa dataset wage2
library("AER") #importa ivreg

data("wage2")

frist_stage <- lm(educ ~ sibs + exper + tenure + black, data = wage2)
summary(frist_stage)
educ_fit <- fitted(frist_stage)

second_stage <- lm ( log(wage) ~ educ_fit + exper + tenure + black, data = wage2)
summary(second_stage)

# Agora 2sls automático

two_stages <- ivreg ( log(wage) ~ educ+ exper + tenure + black | 
                                  sibs+ exper + tenure + black, data = wage2)
summary(two_stages)