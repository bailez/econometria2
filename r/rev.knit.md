---
title: "Estudos em Econometria 2"
author: "Felipe Bailez"
date: "15/06/2021"
output: pdf_document
header-includes:
- \usepackage[brazilian]{babel}
- \usepackage[utf8]{inputenc}
---




## Programa do Curso


\begin{tabular}{ |c|c|c| } 
 \hline
OLS & Cunningham & Cap. 1 e 2\\ 
DAGs & Cunningham & Cap. 3\\ 
Potential Outcomes & Cunningham & Cap. 4\\
Matching & Cunningham & Cap. 1 e 2\\
Discrete Choice Models (Probit/Logit) & Wooldridge & Cap. 17\\ 
Regression Discontinuity & Cunningham & Cap. 6\\
Instrumental Variables & Cunningham & Cap. 7\\
Panel Data & Cunningham & Cap. 7\\
DiD & Cunningham & Cap. 9\\
Two-way FE with differential timing & Cunningham & Cap. 9\\ 
 \hline

\end{tabular}
\newpage
## OLS

Estimando a OLS através de matrizes (Wooldrigde Appendix E):
*$$ y_t = \beta_1 + \beta_2 x_{t2} + \beta_3 x_{t3} + ... + \beta_k x_{tk} + u_t 
, t = 1,2,...,n$$*

Para cada $t$, definimos um vetor $1 \times k$ onde *$ x_t = (1,x_2,...,x_{tk}) $ e $\beta = (\beta_1, \beta_2, ..., \beta_k)'$*,
então

$$ y_t =  x_t \beta + u_t , t = 1,2, ... , n$$


$$ y = X \beta + u $$

Para calcular os coeficientes:

$$ X'(y - X \hat{\beta} )= 0$$
$$ (X'X)\hat{\beta} = X'y $$
$$ \hat{\beta} = (X'X)^{-1} X'y $$

### Exemplo em R

```r
Y <- matrix(c(1.5,6.5,10,11,11.5,16.5), ncol=1, nrow=6, byrow=F)

X <- matrix(c(1,1,1,1,1,1,0,1,1,2,2,3,0,2,4,2,4,6), ncol = 3, nrow = 6, byrow=F)

Xt = t(X)
XtX = Xt %*% X
XtX_inv = solve(XtX)
Xty = Xt %*% Y

beta_hat = XtX_inv %*% Xty
```
\newpage
## Potential Outcomes


```r
# Algum codigo aqui
```

\newpage
## Matching


```r
# Algum codigo aqui
```

\newpage
## Discrete Choice Models


```r
# Algum codigo aqui
```
\newpage
## Regression Discontinuity


```r
# Algum codigo aqui
```
\newpage
## Instrumental Variables

### *Homogenous Treatment Effects (Cunningham Cap. 7)*

Em *Homogenous Treatment Effects*, você supõe que todas as pessoas do grupo que receberam o tratamento terão uma mudança na variável de interesse com a mesma intensidade. Ou seja, se fazer universidade aumento minha renda em 10%, então aumentou em 10% para todos que fizeram universidade.

Portanto, suponha um modelo onde você deseja estimar o quanto um aumento de educação causa aumento na renda.

$$ Y_i = \alpha + \delta S_i + \gamma A_i + \varepsilon_i$$

Onde, $Y_i$ é a renda de cada individuo, $S_i$ os anos de educação e $A_i$ uma variável não observada que representa abilidade. Desse modo, o modelo que conseguiremos estimar é o seguinte:

$$ Y_i = \alpha + \delta S_i + \eta_i$$

onde $\eta_i$ é o erro composto equivalente a $\gamma A_i + \varepsilon_i$. Como assumimos que "abilidade" está correlacionada com a variável de "educação", então apenas $\varepsilon_i$ está descorrelacionado com os regressores.

utilizando o valor estimado de $\hat{\delta}$ da OLS tradicional temos que

$$ \hat{\delta} =  \frac{Cov(Y,S)}{Var(S)} = \frac{E[YS]- E[Y] E[S]}{Var(S)}$$

Se utilizarmos o valor de $Y$ da regressão onde $A$ é observável teremos que

$$ \hat{\delta} =  \frac{E[S (\alpha +\delta S + \gamma A + \epsilon)] - E[\alpha + \delta S + \gamma A + \epsilon] E[S]}{Var(S)}$$
$$ \hat{\delta} =  \frac{\delta E(S^2) - \delta E(S)^2 + \gamma E(AS) - \gamma E(S) E (A) + E(\varepsilon S)+ E(S) E(\varepsilon)}{Var(S)}$$

$$ \hat{\delta} =  \delta + \gamma\frac{Cov(A,S)}{Var(S)}$$
Logo, se $\gamma > 0$ e $Cov(A,S) > 0$, então $\hat{\delta}$ será viesado para cima.E como deve ser positivamente correlacionada com eduação, então isso é o que deve acontecer.

Mas se encontrarmos uma nova variável $Z_i$ que causa as pessoas a ter mais anos de estudos e que é descorrelacionada com abilidade (as variáveis não observáveis), podemos utilizar ela como uma variável instrumental para estimar $\delta$.

Para isso precisamos primeiro calcular a covariancia de $Y$ e $Z$

$$ Cov(Y,Z) = Cov( \alpha + \delta S + \gamma A + \varepsilon, Z)$$


$$ = E[ Z (\alpha + \delta S + \gamma A + \varepsilon )] - E[\alpha + \delta S + \gamma A + \varepsilon ] \ \ E[Z]$$
$$ =  E[\alpha Z + \delta S Z + \gamma A Z + \varepsilon Z ] -  \ \{ \alpha + \delta E(S) + \gamma E(A) + E(\varepsilon)\ \} \ E[Z]$$

$$ =   \{ \alpha E(Z) + \delta E(SZ) + \gamma E (AZ) + E (\varepsilon Z) \}  -\\
 \{\alpha E(Z) + \delta E(S) E(Z) + \gamma E(A) E(Z) + E(\varepsilon) E(Z) \ \} $$

$$ =   \{ \ \alpha E(Z) - \alpha E(Z) \ \} + \delta \{ \ E (SZ) - E(S) \ E(Z) \ \} + \\
\gamma \ \{  E (AZ) - E(A) \ E(Z) \ \} + \\
\{  E (\varepsilon Z) -  E(\varepsilon) \ E(Z) \}$$

$$ =   \delta \ Cov(S,Z) + \gamma \ Cov(A, Z) + Cov \ (\varepsilon, Z)$$

Como sabemos que $Cov(A,Z) = 0$ e $Cov(\varepsilon, Z) = 0$, uma vez que não existe essa relação entre os instrumentos, podemos estimar $\hat{\delta}$.

$$ \hat{\delta} = \frac{Cov(Y,Z)}{Cov(S,Z)} $$
Dessa forma, podemos usar a variável instrumental $Z$ para estimar $\hat{\delta}$ caso $Z$ seja independente da variável oculta e do erro estrutural da regressão. Ou seja, o instrumento deve ser independente das duas partes do erro composto $\eta_i$ citado no início.



```r
Y <- matrix(c(1.5,6.5,10,11,11.5,16.5), ncol=1, nrow=6, byrow=F)
X <- matrix(c(1,1,1,1,1,1,0,1,1,2,2,3,0,2,4,2,4,6), ncol = 3, nrow = 6, byrow=F)
```

### *Two-stage least squares*

Uma forma mais intuitiva de trabalhar com Variáveis Instrumentais é através das *Two-stage least squares* (ou $2SLS$).
Seguindo o raciocínio de antes, suponha que temos dados de $Y$, $S$ e $Z$ para cada observação $i$. Nesse caso o processo de geração de dados é dado por:

$$ Y_i = \alpha + \delta S_i + \varepsilon_i$$

$$ S_i = \gamma + \beta Z_i + \epsilon_i$$

onde $ Cov(Z,\varepsilon) = 0 $ e $\beta \neq 0 $ . Sabendo que $\sum_{i=1}^n (X-i \bar{x}) = 0$, podemos reescrever o estimador de variável instrumental como

$$ \hat{\delta} = \frac{Cov(Y,Z)}{Cov(S,Z)}$$
$$ = \frac{ \frac{1}{n} \sum_{i=1}^n (Z_i - \bar{Z} ) (Y_i - \bar{Y}) } { \frac {1} {n} \sum_{i=1}^n (Z_i - \bar{Z}) (S_i - \bar{S}) }$$
$$ = \frac{ \frac{1}{n} \sum  } {\frac{1}{n} } $$

\newpage
## Panel Data


```r
# Algum codigo aqui
```
\newpage
## Diff-in-Diff


```r
# Algum codigo aqui
```
\newpage
## Two-way Fixed-Effects with differential timing


```r
# Algum codigo aqui
```
