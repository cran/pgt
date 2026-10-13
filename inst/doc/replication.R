## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(pgt)

## -----------------------------------------------------------------------------
data(pigfarms)
pigfarms

## -----------------------------------------------------------------------------
v <- 7 / 6
u_feed <- (pigfarms$uncontrolled + v * pigfarms$meat) / pigfarms$feed
tech <- pgt_tech(
  x = pigfarms[, c("feed", "piglet", "labor", "capital")],
  y = pigfarms$meat,
  b = pigfarms$controlled,
  u = cbind(u_feed, 0, 0, 0),
  v = v,
  a = pigfarms$abatement,
  id = pigfarms$farm
)
fit <- pgt(tech, model = "wgd")
data.frame(farm = fit$results$id, b_star = fit$results$b_star,
           z_star = fit$results$z_star, a_star = fit$results$a_star)

## -----------------------------------------------------------------------------
fdmo <- pgt(tech, model = "fdmo")
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
u_c <- c(feed = 31 / 25, piglet = 349 / 300, labor = 0, capital = 0)
x_pig <- as.matrix(pigfarms[, names(u_c)])
z_c <- as.vector(x_pig %*% u_c) - v * pigfarms$meat
a_c <- z_c - pigfarms$controlled
c(uncontrolled = all(round(z_c, 1) == pigfarms$uncontrolled),
  abatement = all(round(a_c, 1) == pigfarms$abatement))
rc <- pgt(pgt_tech(x = x_pig, y = pigfarms$meat, b = pigfarms$controlled,
                   u = u_c, v = v, a = a_c, id = pigfarms$farm),
          model = "fdmo")$results
data.frame(farm = rc$id, gross = round(rc$gross, 3),
           good_eff = round(rc$good_eff, 3),
           bad_eff = round(rc$bad_eff, 3),
           maximal_y = round(rc$maximal_y, 3))

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
           emission_eff = round(rb$efficiency, 4), mean_eff = round(rb$mean_eff, 4))

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

