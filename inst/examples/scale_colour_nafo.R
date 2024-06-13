
library(ggplot2)
showtext::showtext_opts(dpi = 96) # Adjust dpi to match your plotting device (e.g., set to 300 if using ggsave with dpi set to 300)
theme_set(theme_nafo()) # set default ggplot2 theme

set.seed(123)

years <- 2015:2020
mids <- seq(7, 13, length = length(years))
age <- seq(0, 20, by = 0.5)

sel_curves <- lapply(seq_along(years), function(y) {
    sel <- 1 / (1 + exp(-1 * (age - mids[y])))
    data.frame(year = years[y], age = age, sel = sel)
})
sel_curves <- do.call(rbind, sel_curves)

sel_plot <- ggplot(sel_curves, aes(x = age, y = sel, colour = as.factor(year))) +
    geom_line() +
    scale_y_continuous(name = "Selectivity", expand = expansion(c(0.01, 0.01))) +
    scale_x_continuous(name = "Age", expand = expansion(c(0.01, 0.01))) +
    scale_colour_nafo(palette = "blue2green", name = "Year") +
    theme(legend.position = "right",
          legend.box.background = element_blank())
sel_plot

res <- expand.grid(age = 1:10, year = 1990:2020)
res$res <- rnorm(nrow(res))
res_plot <- ggplot(res, aes(x = year, y = age, size = res, fill = res)) +
    geom_point(shape = 21, colour = "#f0f0f0", stroke = .nafo_lwd) +
    scale_size_area(max_size = 8, name = "Residual") +
    scale_fill_nafo(type = "continuous", palette = "hot_cold", name = "Residual") +
    scale_y_continuous(name = "Age", n.breaks = 10) +
    scale_x_continuous(name = "Year", n.breaks = 10) +
    theme(legend.position = "right",
          legend.box.background = element_blank())
res_plot

if (interactive()) {
    showtext::showtext_opts(dpi = 300)
    ggsave("selectivity_plot.png", plot = sel_plot, height = .nafo_height, width = .nafo_width)
    ggsave("residual_plot.png", plot = res_plot, height = .nafo_height, width = .nafo_width)
}

