## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6,
                      fig.height = 4)

## ----setup--------------------------------------------------------------------
library(pgt)

## -----------------------------------------------------------------------------
set.seed(42)
L <- 40
coal <- runif(L, 20, 60)
gas <- runif(L, 10, 40)
y <- runif(L, 15, 45)
u_co2 <- c(coal = 2.5, gas = 1.4)
u_so2 <- c(coal = 0.020, gas = 0.001)
cap_co2 <- 2.5 * coal + 1.4 * gas - 0.1 * y
cap_so2 <- 0.020 * coal + 0.001 * gas
b_co2 <- cap_co2 * runif(L, 0.55, 0.95)
b_so2 <- cap_so2 * runif(L, 0.55, 0.95)
# three plants under-report scrubbing and breach their SO2 account
b_so2[1:3] <- cap_so2[1:3] * runif(3, 1.05, 1.25)

tech2 <- pgt_tech(
  x = cbind(coal = coal, gas = gas),
  y = y,
  b = cbind(co2 = b_co2, so2 = b_so2),
  u = list(co2 = u_co2, so2 = u_so2),
  v = c(co2 = 0.1, so2 = 0),
  group = factor(rep(c("A", "B"), each = L / 2))
)
tech2

## -----------------------------------------------------------------------------
mb <- mb_check(tech2)
table(subset(as.data.frame(mb), violated)$pollutant)

## -----------------------------------------------------------------------------
fit_co2 <- pgt(tech2, model = "wgd_input_fixed", pollutant = "co2")
summary(fit_co2)

## -----------------------------------------------------------------------------
tech1 <- pgt_tech(
  x = cbind(coal = coal, gas = gas), y = y, b = b_co2, u = u_co2,
  v = 0.1, group = factor(rep(c("A", "B"), each = L / 2))
)
fit1 <- pgt(tech1, model = "wgd_input_fixed")
summary(fit1$results$b_star - fit_co2$results$b_star)

## -----------------------------------------------------------------------------
head(cbind(fit_co2$results[c("id", "efficiency", "status")],
           so2_violated = mb$violated[mb$pollutant == "so2"]), 5)

## -----------------------------------------------------------------------------
U_co2 <- matrix(u_co2, L, 2, byrow = TRUE,
                dimnames = list(NULL, c("coal", "gas")))
U_co2[1:20, "coal"] <- 2.8
tech_het <- pgt_tech(
  x = cbind(coal = coal, gas = gas), y = y,
  b = pmin(b_co2, (2.8 * coal + 1.4 * gas - 0.1 * y) * 0.95),
  u = U_co2, v = 0.1
)
fit_het <- pgt(tech_het, model = "wgd_input_fixed")
median(fit_het$results$efficiency, na.rm = TRUE)

## -----------------------------------------------------------------------------
dec <- pgt_decompose(tech_het, type = "rodseth")
summary(dec)

## -----------------------------------------------------------------------------
y2 <- cbind(power = y, heat = runif(L, 5, 20))
tech_m <- pgt_tech(
  x = cbind(coal = coal, gas = gas),
  y = y2,
  b = b_co2,
  u = u_co2,
  v = c(0.10, 0.05)
)
fit_m <- pgt(tech_m, model = "wgd")
head(fit_m$results, 3)

