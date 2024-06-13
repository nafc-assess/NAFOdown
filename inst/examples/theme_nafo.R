
library(ggplot2)
library(patchwork)
showtext::showtext_opts(dpi = 96) # Adjust dpi to match your plotting device (e.g., set to 300 if using ggsave with dpi set to 300)
theme_set(theme_nafo()) # set default ggplot2 theme

C_plot <- ggplot(aes(x = year), data = toy_stock) +
    geom_bar(aes(y = catch, fill = "Catch"), stat = "identity", alpha = 0.4) +
    geom_step(aes(x = year - 0.5, y = ifelse(year > 2015, 0, TAC), linetype = "TAC"), color = .nafo_cols[2]) +
    scale_y_continuous(n.breaks = 10, name = "Catch / TAC", expand = expansion(c(0.01, 0.05))) +
    scale_x_continuous(n.breaks = 10, name = "", expand = expansion(c(0.01, 0.01))) +
    scale_fill_nafo() +
    theme(legend.title = element_blank(),
          legend.spacing.y = unit(0.1, "cm"))

B_plot <- ggplot(aes(x = year), data = toy_stock) +
    geom_ribbon(aes(ymin = relB_lwr, ymax = relB_upr), fill = .nafo_cols[1], alpha = 0.4) +
    geom_line(aes(y = relB), color = .nafo_cols[1]) +
    scale_y_continuous(limits = c(0, NA), n.breaks = 10, name = "B/Blim", expand = expansion(c(0.01, 0.05))) +
    scale_x_continuous(n.breaks = 10, name = "", expand = expansion(c(0.01, 0.01)))

F_plot <- ggplot(aes(x = year), data = toy_stock) +
    geom_ribbon(aes(ymin = relF_lwr, ymax = relF_upr), fill = .nafo_cols[1], alpha = 0.4) +
    geom_line(aes(y = relF), color = .nafo_cols[1]) +
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

(C_plot + B_plot) / (F_plot + R_plot)

if (interactive()) {
    showtext::showtext_opts(dpi = 300)
    ggsave("summary_plot.png", height = .nafo_height * 2, width = .nafo_width * 2)
}

