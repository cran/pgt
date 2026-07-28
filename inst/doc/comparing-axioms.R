## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6,
                      fig.height = 4)

## ----setup--------------------------------------------------------------------
library(pgt)

## -----------------------------------------------------------------------------
data(steeldemo)
tech <- pgt_tech(
  x = steeldemo[, c("coal_coke", "other_fuel", "raw_material", "flux")],
  y = steeldemo$production,
  b = steeldemo$emissions,
  v = 0.01467,
  group = steeldemo$route,
  id = steeldemo$plant
)

fits <- lapply(c("wgd", "byprod", "mb_cost", "wd"), function(m) {
  pgt(tech, model = m)
})
names(fits) <- c("wgd", "byprod", "mb_cost", "wd")
sapply(fits, function(f) round(median(f$results$efficiency, na.rm = TRUE), 3))

## -----------------------------------------------------------------------------
cmp <- compare_models(tech, models = c("wgd", "byprod", "mb_cost", "wd"))
cmp

## ----fig.alt = "Line plot of efficiency profiles under four models, with plants ordered by the first model's score and one line per model; where lines cross, the models disagree on a plant's rank"----
plot(cmp)

