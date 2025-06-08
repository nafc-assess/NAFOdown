
library(ggplot2)
library(patchwork)
showtext::showtext_opts(dpi = 96) # Adjust dpi to match your plotting device (e.g., set to 300 if using ggsave with dpi set to 300)
theme_set(theme_nafo()) # set default ggplot2 theme

## Example of a summary plot ---------------------------------------------------

C_plot <- ggplot(aes(x = year), data = toy_stock) +
    geom_bar(aes(y = catch, fill = "Catch"), stat = "identity", alpha = 0.4) +
    geom_step(aes(x = year - 0.5, y = ifelse(year > 2015, 0, TAC), linetype = "TAC"), color = .nafo_cols[2]) +
    scale_y_continuous(n.breaks = 10, name = "Catch / TAC", expand = expansion(c(0.01, 0.05))) +
    scale_x_continuous(n.breaks = 10, name = "", expand = expansion(c(0.01, 0.01))) +
    scale_fill_nafo() +
    theme(legend.title = element_blank(),
          legend.spacing.y = unit(0.1, "cm"))

B_plot <- ggplot(aes(x = year), data = toy_stock) +
    geom_ribbon(aes(ymin = Brel_lwr, ymax = Brel_upr), fill = .nafo_cols[1], alpha = 0.4) +
    geom_line(aes(y = Brel), color = .nafo_cols[1]) +
    scale_y_continuous(limits = c(0, NA), n.breaks = 10, name = "B/Blim", expand = expansion(c(0.01, 0.05))) +
    scale_x_continuous(n.breaks = 10, name = "", expand = expansion(c(0.01, 0.01)))

F_plot <- ggplot(aes(x = year), data = toy_stock) +
    geom_ribbon(aes(ymin = Frel_lwr, ymax = Frel_upr), fill = .nafo_cols[1], alpha = 0.4) +
    geom_line(aes(y = Frel), color = .nafo_cols[1]) +
    scale_y_continuous(limits = c(0, NA), n.breaks = 10, name = "F/Flim", expand = expansion(c(0.01, 0.05))) +
    scale_x_continuous(n.breaks = 10, name = "", expand = expansion(c(0.01, 0.01)))

R_plot <- ggplot(aes(x = year), data = toy_stock) +
    geom_line(aes(y = rec_index1, color = "Survey 1")) +
    geom_line(aes(y = rec_index2, color = "Survey 2")) +
    geom_line(aes(y = rec_index3, color = "Survey 3")) +
    scale_y_continuous(limits = c(0, NA), name = "Recruitment index", expand = expansion(c(0.01, 0.05))) +
    scale_x_continuous(n.breaks = 10, name = "", expand = expansion(c(0.01, 0.01))) +
    scale_color_nafo() +
    theme(legend.title = element_blank())

summary_plot <- (C_plot + B_plot) / (F_plot + R_plot)
summary_plot


## Facet plot of multiple species ----------------------------------------------

set.seed(123)

years <- rep(1990:2020, times = 4)
species <- rep(c("Cod", "Plaice", "Redfish", "Halibut"), each = length(unique(years)))
biomass <- exp(cumsum(rnorm(length(years), sd = 0.1)))
error <- exp(rnorm(length(years), mean = log(0.1), sd = 0.5))
lwr <- exp(log(biomass) - error)
upr <- exp(log(biomass) + error)
species_trends <- data.frame(year = years, species = species, biomass = biomass, lwr = lwr, upr = upr)
species_trends[species_trends$year %in% sample(unique(years), 5), c("biomass", "lwr", "upr")] <- NA

species_plot <- ggplot(aes(x = year, color = species), data = species_trends) +
    geom_line(aes(y = biomass), linewidth = .nafo_lwd, linetype = 3) +
    geom_segment(aes(xend = year, y = lwr, yend = upr), linewidth = .nafo_lwd) +
    geom_point(aes(y = biomass), shape = 21, fill = "white") +
    facet_wrap(~ species, scales = "free") +
    scale_y_continuous(limits = c(0, NA), name = "Biomass index", expand = expansion(c(0.01, 0.05))) +
    scale_x_continuous(n.breaks = 10, name = "Year", expand = expansion(c(0.02, 0.02))) +
    scale_color_nafo(name = "Species") +
    theme(legend.position = "none")
species_plot

if (interactive()) {
    showtext::showtext_opts(dpi = 300)
    ggsave("summary_plot.png", plot = summary_plot, height = .nafo_height * 2, width = .nafo_width * 2)
    ggsave("species_plot.png", plot = species_plot, height = .nafo_height * 2, width = .nafo_width * 2)
}

