library("tidyverse")
library("haven")
library("rlang")
library("broom")
library("lmtest")
library("sandwich")
install.packages("devtools")
devtools::install_github("jrnold/masteringmetrics", subdir = "masteringmetrics")



data("mlda", package = "masteringmetrics")

mlda <- mutate(mlda,
               age = agecell - 21,
               over21 = as.integer(agecell >= 21))

mlda <- mutate(mlda, ext_oth = external - homicide - suicide - mva)


varlist <- c("all" = "All Causes",
             "mva" = "Motor Vehicle Accidents",
             "internal" = "Internal Causes")

mlda %>%
  select(agecell, over21, one_of(names(varlist))) %>%
  gather(response, value, -agecell, -over21, na.rm = TRUE) %>%
  mutate(response = recode(response, !!!as.list(varlist))) %>%
  ggplot(aes(x = agecell, y = value)) +
  geom_point() +
  geom_smooth(mapping = aes(group = over21), se = FALSE, method = "lm",
              formula = y ~ poly(x, 2)) +
  geom_smooth(mapping = aes(group = over21), se = FALSE, method = "lm",
              formula = y ~ x, color = "black") +
  facet_grid(response ~ ., scales = "free_y") +
  labs(y = "Mortality rate (per 100,000)", x = "Age")


responses <- c("all" = "All deaths",
               "mva" = "Motor vehicle accidents",
               "suicide" = "Suicide",
               "homicide" = "Homocide",
               "ext_oth" = "Other external causes",
               "internal" = "All internal causes",
               "alcohol" = "Alcohol")








run_reg <- function(y) {
  mods <- list(
    "Ages 19-22, Linear" =
      lm(quo(!!sym(y) ~ age * over21), data = mlda),
    "Ages 19-22, Quadratic" =
      lm(quo(!!sym(y) ~ poly(age, 2, raw = TRUE) * over21), data = mlda),
    "Ages 20-21, Linear" =
      lm(quo(!!sym(y) ~ age * over21),
         data = filter(mlda, agecell >= 20, agecell <= 22)),
    "Ages 20-21, Quadratic" =
      lm(quo(!!sym(y) ~ poly(age, 2, raw = TRUE) * over21),
         data = filter(mlda, agecell >= 20, agecell <= 22))
  )
  out <- tibble(
    model_name = names(mods),
    model = mods,
    ages = rep(c("19-22", "20-21"), each = 2),
    trend = rep(c("Linear", "Quadratic"), 2),
    model_num = seq_along(mods)
  ) %>%
    mutate(coefs = map(model, ~ tidy(coeftest(.x, vcovHC(.x))))) %>% # nolint
    unnest(coefs, .drop = FALSE) %>%
    filter(term == "over21") %>%
    select(model_name, model, term, estimate, std.error) %>%
    mutate(response = y)
  # sample size = df.residuals + residuals
  out[["obs"]] <- map_dfr(mods, glance) %>%
    mutate(obs = df.residual + df) %>%
    pluck("obs")
  out
}

mlda_regs <- map_dfr(names(responses), run_reg) %>%
  mutate(response = recode(response, !!!as.list(responses)))


mlda_regs %>%
  select(model_name, response, estimate, std.error) %>%
  gather(stat, value, estimate, std.error) %>%
  spread(model_name, value) %>%
  knitr::kable()