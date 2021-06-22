################################ RDD SIMULATE 1 ################################

library(tidyverse)

# simulate the data
dat <- tibble(
  x = rnorm(1000, 50, 25)
) %>%
  mutate(
    x = if_else(x < 0, 0, x)
  ) %>%
  filter(x < 100)

x <- dat$x
# cutoff at x = 50
dat <- dat %>% 
  mutate(
    D  = if_else(x > 50, 1, 0),
    y1 = 25 + 0 * D + 1.5 * x + rnorm(n(), 0, 20)
  )

ggplot(aes(x, y1, colour = factor(D)), data = dat) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 50, colour = "grey", linetype = 2)+
  stat_smooth(method = "lm", se = F) +
  labs(x = "Test score (X)", y = "Potential Outcome (Y1)")


################################ RDD SIMULATE 2 ################################

# simulate the discontinuity
dat <- dat %>%
  mutate(
    y2 = 25 + 40 * D + 1.5 * x + rnorm(n(), 0, 20)
  )

# figure
ggplot(aes(x, y2, colour = factor(D)), data = dat) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 50, colour = "grey", linetype = 2) +
  stat_smooth(method = "lm", se = F) +
  labs(x = "Test score (X)", y = "Potential Outcome (Y)")

################################ RDD SIMULATE 3 ################################

# simultate nonlinearity
dat <- tibble(
  x = rnorm(1000, 100, 50)
) %>% 
  mutate(
    x = case_when(x < 0 ~ 0, TRUE ~ x),
    D = case_when(x > 140 ~ 1, TRUE ~ 0),
    x2 = x*x,
    x3 = x*x*x,
    y3 = 10000 + 0 * D - 100 * x + x2 + rnorm(1000, 0, 1000)
  ) %>% 
  filter(x < 280)


ggplot(aes(x, y3, colour = factor(D)), data = dat) +
  geom_point(alpha = 0.2) +
  geom_vline(xintercept = 140, colour = "grey", linetype = 2) +
  stat_smooth(method = "lm", se = F) +
  labs(x = "Test score (X)", y = "Potential Outcome (Y)")

ggplot(aes(x, y3, colour = factor(D)), data = dat) +
  geom_point(alpha = 0.2) +
  geom_vline(xintercept = 140, colour = "grey", linetype = 2) +
  stat_smooth(method = "loess", se = F) +
  labs(x = "Test score (X)", y = "Potential Outcome (Y)")

################################ RDD SIMULATE 4 ################################

library(stargazer)

dat <- tibble(
  x = rnorm(1000, 100, 50)
) %>% 
  mutate(
    x = case_when(x < 0 ~ 0, TRUE ~ x),
    D = case_when(x > 140 ~ 1, TRUE ~ 0),
    x2 = x*x,
    x3 = x*x*x,
    y3 = 10000 + 0 * D - 100 * x + x2 + rnorm(1000, 0, 1000)
  ) %>% 
  filter(x < 280)

regression <- lm(y3 ~ D*., data = dat)

stargazer(regression, type = "text") 

ggplot(aes(x, y3, colour = factor(D)), data = dat) +
  geom_point(alpha = 0.2) +
  geom_vline(xintercept = 140, colour = "grey", linetype = 2) +
  stat_smooth(method = "loess", se = F) +
  labs(x = "Test score (X)", y = "Potential Outcome (Y)")