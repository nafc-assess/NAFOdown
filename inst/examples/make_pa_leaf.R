
## PA calculation and plotting code was developed by Andie Perreault (NAFO Secretariat),
## Laura Wheeland, Paul Regular, and Mariano Koen-Alonso

library(ggplot2)
library(NAFOdown)
showtext::showtext_opts(dpi = 300)

## Basic leaf plot ----------

Bmsy <- 1
Blim <- 0.3 * Bmsy
Btrigger <- 0.75 * Bmsy
Fmsy <- 1
Flim <- Fmsy
Ftarget <- 0.85 * Fmsy

pa_leaf <- plot_PA_leaf(data = NULL, Blim = Blim, Btrigger = Btrigger,
                        Ftarget = Ftarget, Flim = Flim)
pa_leaf
ggsave("PA_leaf.png", dpi = 300,
       width = .nafo_width, height = .nafo_height * 1.5)


## Leaf plot with stock trends ----------

## Using simulated data built into NAFOdown (see ?NAFOdown::toy_stock)
sim_data <- with(toy_stock, data.frame(year = year, Btrend = Brel, Ftrend = Frel,
                                       Blwr = Brel_lwr, Bupr = Brel_upr,
                                       Flwr = Frel_lwr, Fupr = Frel_upr))

trends_leaf <- plot_PA_leaf(sim_data, Blim = Blim, Btrigger = Btrigger,
                            Ftarget = Ftarget, Flim = Flim)
trends_leaf
ggsave("PA_leaf_with_history.png", dpi = 300,
       width = .nafo_width * 1.5, height = .nafo_height * 1.5)


## Leaf plot with projections ----------

## Build idealized path following leaf
years <- 2020:2024
Bseq <- seq(0.4, 0.6, length.out = 5)
Fmid <- F_linear(Bseq, Blim, Btrigger, Ftarget)
Flower <- F_lower(Bseq, Blim, Btrigger, Ftarget)
Fupper <- F_upper(Bseq, Blim, Btrigger, Ftarget)
proj_data <- data.frame(year = rep(years, 3), Btrend = rep(Bseq, 3),
                        Ftrend = c(Fmid, Flower, Fupper),
                        scenario = c(rep("Mid", 5), rep("Lower", 5), rep("Upper", 5)))
proj_data$Ftrend[proj_data$year == 2020][] <- proj_data$Ftrend[proj_data$year == 2020 & proj_data$scenario == "Mid"]
proj_data$Ftrend[proj_data$year == 2021][] <- proj_data$Ftrend[proj_data$year == 2021 & proj_data$scenario == "Mid"]
proj_data$scenario <- factor(proj_data$scenario, levels = c("Lower", "Mid", "Upper"))
proj_data$label_just <- ifelse(proj_data$year == min(proj_data$year), "left", "right")
proj_data

proj_leaf <- plot_PA_leaf(proj_data, Blim = Blim, Btrigger = Btrigger,
                          Ftarget = Ftarget, Flim = Flim)
proj_leaf
ggsave("PA_leaf_with_projections.png", dpi = 300,
       width = .nafo_width, height = .nafo_height * 1.5)


## Need to zoom in? ----------

## Use coord_cartesian, but don't zoom in too far such that only one Zone is visible
proj_leaf + coord_cartesian(xlim = c(0.3, 0.9)) # ok
proj_leaf + coord_cartesian(xlim = c(0.3, 0.75)) # not ok


## Need to change legend position? ----------

## Bottom left
proj_leaf +
    theme(legend.position.inside = c(0.05, 0.05),
          legend.justification = c(0, 0))

## Top left
proj_leaf +
    theme(legend.position.inside = c(0.05, 0.9),
          legend.justification = c(0, 1))

## Top right
proj_leaf +
    theme(legend.position.inside = c(0.95, 0.9),
          legend.justification = c(1, 1))


## More manual editing required? ----------

## To modify the full plot structure, type `plot_PA_leaf` and press Enter to view formatted code.
## Then copy and paste that code into your R script, modify the names used in the `make_pa_data`
## function to match the names you've used, then manually edit the ggplot code to suit your needs.
plot_PA_leaf

