# 4. For this exercise, we use JTRAIN.RAW to determine the effect of the job training
# grant on hours of job training per employee. The basic model for the three years is:
# 
#     hrsempit = β0 + δ1d88t + δ2d89t + β1grantit + β2granti,t−1 + β3log(employit) + ai + uit
# 
# (a) Estimate the equation using first differencing.

library("wooldridge") #importar jtrain
library("plm")

data('jtrain')


pdim(jtrain) # usa o pacote plm para medir as dimensões

jtrain_panel <- pdata.frame(jtrain) # transforma o indice em data

reg <- lm(hrsemp ~ d88 + d89 + grant + grant_1 + log(employ),  #OLS normal
              data = jtrain_panel)
summary(reg)



reg_fd <- plm(hrsemp ~  grant + grant_1 + log(employ), #OLS Com primeiras diferenças
                  data = jtrain_panel, 
                  model = "fd")
summary(reg_fd)
