## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6,
                      fig.height = 4)

## ----setup--------------------------------------------------------------------
library(pgt)

## -----------------------------------------------------------------------------
data(steeldemo)
tech <- pgt_tech(
  x = steeldemo[, c("coal_coke", "other_fuel", "raw_material", "flux",
                    "capture_energy")],
  y = steeldemo$production,
  b = steeldemo$emissions,
  a = steeldemo$captured,
  v = 0.01467,
  x_abate = "capture_energy",
  group = steeldemo$route,
  id = steeldemo$plant
)
tech

## -----------------------------------------------------------------------------
mb <- mb_check(tech)
head(as.data.frame(mb))
attr(mb, "n_violations")

## -----------------------------------------------------------------------------
fit <- pgt(tech, model = "wgd")
summary(fit)

## -----------------------------------------------------------------------------
fit_env <- pgt(tech, model = "envelope")
summary(fit_env)

## -----------------------------------------------------------------------------
fit_dir <- pgt(tech, model = "fdmo")
summary(fit_dir)

## ----fig.alt = "Step curve of marginal abatement cost against cumulative abatement potential"----
head(shadow_prices(fit))
mac <- mac_curve(fit, price = 550)
plot(mac)

## -----------------------------------------------------------------------------
dec <- pgt_decompose(tech, type = "envelope")
summary(dec)

## -----------------------------------------------------------------------------
dec5 <- pgt_decompose(tech, type = "rodseth")
summary(dec5)
comps <- c("te_production", "quality", "ae_production",
           "te_abatement", "ae_abatement", "total")
med <- aggregate(dec5$results[comps],
                 list(abatement = steeldemo$abatement_tech), median)
med[comps] <- round(med[comps], 3)
med

## ----eval = requireNamespace("frontier", quietly = TRUE)----------------------
data("riceProdPhil", package = "frontier")
d8 <- riceProdPhil[riceProdPhil$YEARDUM == 8, ]
uN <- 0.46
rice <- pgt_tech(
  x = as.matrix(d8[, c("AREA", "LABOR", "NPK", "OTHER")]),
  y = d8$PROD,
  b = uN * d8$NPK,
  u = c(AREA = 0, LABOR = 0, NPK = uN, OTHER = 0),
  v = 0,
  id = as.character(d8$FMERCODE)
)
rice_fit <- pgt(rice, model = "wgd")
summary(rice_fit)

