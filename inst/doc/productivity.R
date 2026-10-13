## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6,
                      fig.height = 4)

## ----setup--------------------------------------------------------------------
library(pgt)

## -----------------------------------------------------------------------------
set.seed(1)
n <- 15
d1 <- data.frame(id = 1:n, y = runif(n, 5, 10), x = runif(n, 8, 12),
                 b = runif(n, 2, 6))
d2 <- data.frame(id = 1:n, y = d1$y * 1.03, x = d1$x, b = d1$b * 0.85)
d <- rbind(cbind(d1, period = 1), cbind(d2, period = 2))
panel <- pgt_tech(x = d[, "x", drop = FALSE], y = d$y, b = d$b,
                  period = d$period, id = d$id)
ml <- pgt_ml(panel)
summary(ml)

## ----eval = requireNamespace("frontier", quietly = TRUE)----------------------
data("riceProdPhil", package = "frontier")
r <- riceProdPhil
uN <- 0.46
rice_panel <- pgt_tech(
  x = as.matrix(r[, c("AREA", "LABOR", "NPK", "OTHER")]),
  y = r$PROD,
  b = uN * r$NPK,
  u = c(AREA = 0, LABOR = 0, NPK = uN, OTHER = 0),
  v = 0,
  period = r$YEARDUM,
  id = as.character(r$FMERCODE)
)
rice_ml <- pgt_ml(rice_panel)
summary(rice_ml)

## -----------------------------------------------------------------------------
data(steeldemo)
steel60 <- steeldemo[steeldemo$year == 2021, ]
tech <- pgt_tech(
  x = steel60[, c("coal_coke", "other_fuel", "raw_material", "flux",
                  "capture_energy")],
  y = steel60$production, b = steel60$emissions, a = steel60$captured,
  v = 0.01467, x_abate = "capture_energy",
  group = steel60$route, id = steel60$plant
)
bt <- boot_pgt(tech, model = "wgd", B = 40, seed = 1)
head(bt$per_dmu)

## -----------------------------------------------------------------------------
boot_pgt_sensitivity(tech, model = "wgd", B = 25,
                     m_grid = c(15, 25, 40), seed = 1)

## ----echo = FALSE-------------------------------------------------------------
cov <- read.csv(system.file("simulations", "coverage-results.csv",
                            package = "pgt"))
dflt <- abs(cov$kappa - 2 / 3) < 1e-9 & cov$m == round(cov$L^0.7)
knitr::kable(
  cov[dflt, c("dgp", "L", "available", "coverage_cond",
              "coverage_delivered", "coverage_frontier",
              "coverage_interior", "coverage_group_means")],
  digits = 3, row.names = FALSE,
  col.names = c("frontier", "L", "available", "given interval",
                "delivered", "frontier (theta > 0.9)", "interior",
                "group means")
)

