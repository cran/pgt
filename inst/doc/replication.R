## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(pgt)

## -----------------------------------------------------------------------------
data(pigfarms)
pigfarms

## -----------------------------------------------------------------------------
tech <- pgt_tech(
  x = pigfarms[, c("uncontrolled", "labor", "capital")],
  y = pigfarms$meat,
  b = pigfarms$controlled,
  u = c(1, 0, 0),
  a = pigfarms$abatement,
  id = pigfarms$farm
)
fit <- pgt(tech, model = "wgd")
data.frame(farm = fit$results$id, b_star = fit$results$b_star)

## -----------------------------------------------------------------------------
v <- 7 / 6
u_feed <- (pigfarms$uncontrolled + v * pigfarms$meat) / pigfarms$feed
tech3 <- pgt_tech(
  x = pigfarms[, c("feed", "piglet", "labor", "capital")],
  y = pigfarms$meat,
  b = pigfarms$controlled,
  u = cbind(u_feed, 0, 0, 0),
  v = v,
  a = pigfarms$abatement,
  id = pigfarms$farm
)
fdmo <- pgt(tech3, model = "fdmo")
r3 <- fdmo$results
comparison <- data.frame(
  farm = r3$id,
  good_eff = round(r3$good_eff, 3),
  paper_good = c(0, 0, 3, 0, 0.8),
  bad_eff = round(r3$bad_eff, 3),
  paper_bad = c(0, 0, 3.5, 0, 1.0),
  maximal_y = round(r3$maximal_y, 3)
)
comparison

## -----------------------------------------------------------------------------
mrl <- pgt_tech(
  x = matrix(c(1, 1, 1, 2, 2), ncol = 1, dimnames = list(NULL, "x")),
  y = c(2, 3 / 2, 2 / 3, 3, 2),
  b = c(4, 1, 2, 5, 3),
  u = 1, polluting = "x",
  id = paste0("DMU", 1:5)
)
bp <- pgt(mrl, model = "byprod", returns = "crs")
rb <- bp$results
data.frame(id = rb$id, output_eff = round(rb$output_eff, 4),
           emission_eff = round(rb$efficiency, 4), fgl = round(rb$fgl, 4))

## -----------------------------------------------------------------------------
farms <- data.frame(
  feed = c(200, 210, 190, 205),
  piglet = c(20, 22, 18, 21),
  meat = c(100, 100, 95, 102)
)
tech_c <- pgt_tech(
  x = as.matrix(farms[, c("feed", "piglet")]),
  y = farms$meat,
  b = 0.0124 * farms$feed + 0.0117 * farms$piglet - 0.0117 * farms$meat,
  u = c(feed = 0.0124, piglet = 0.0117),
  v = 0.0117
)
mbc <- pgt(tech_c, model = "mb_cost", returns = "crs")
rc <- mbc$results
data.frame(id = rc$id, EE = round(rc$efficiency, 4),
           TE = round(rc$te, 4), EAE = round(rc$eae, 4))
all.equal(rc$efficiency, rc$te * rc$eae)

