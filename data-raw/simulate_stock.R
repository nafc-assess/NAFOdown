
## Really simple surplus production simulation to generate some data for plotting

sim_stock <- function(r, K, B0, q1, q2, q3, a, b, years, TAC_interval,
                      sigma_biomass, sigma_TAC, sigma_catch, sigma_index, sigma_rec) {

    Fmsy <- r / 2
    Bmsy <- K / 2
    year <- 1:years
    TAC_year <- seq(1, years, TAC_interval)
    biomass <- catch <- TAC <- numeric(years)
    biomass[1] <- B0
    TAC[1] <- 0.85 * Fmsy * biomass[1] # Initialize TAC at MSY
    catch[1] <- rlnorm(1, log(TAC[1]), sigma_catch)

    for (t in 2:years) {
        surplus_production <- r * biomass[t-1] * (1 - biomass[t-1] / K)
        if (t %in% TAC_year) {
            TAC[t] <- TAC[t-1] + rnorm(1, 0, sigma_TAC) # Random walk TAC
        } else {
            TAC[t] <- TAC[t-1]
        }
        catch[t] <- rlnorm(1, log(TAC[t]), sigma_catch)
        biomass[t] <- biomass[t-1] + surplus_production - catch[t]
        biomass[t] <- pmax(biomass[t], 0)
        biomass[t] <- rlnorm(1, log(biomass[t]), sigma_biomass)
    }

    rec <- rlnorm(length(biomass), log((a * biomass) / (1 + b * biomass)), sigma_rec) # BH stock-rec
    rec_index1 <- rlnorm(length(biomass), log(q1 * rec), sigma_index)
    rec_index2 <- rlnorm(length(biomass), log(q2 * rec), sigma_index * 2)
    rec_index3 <- rlnorm(length(biomass), log(q3 * rec), sigma_index * 3)

    data.frame(
        year = year,
        biomass = biomass,
        rec_index1 = rec_index1,
        rec_index2 = rec_index2,
        rec_index3 = rec_index3,
        TAC = TAC,
        catch = catch,
        Fmsy = Fmsy,
        Bmsy = Bmsy
    )

}


set.seed(32)

stock <- sim_stock(
    r = 0.3,
    K = 1000,
    B0 = 500,
    q1 = 0.03,
    q2 = 0.02,
    q3 = 0.01,
    a = 5,
    b = 0.001,
    years = 40,
    TAC_interval = 5,
    sigma_biomass = 0.2,
    sigma_TAC = 100,
    sigma_catch = 0.2,
    sigma_index = 0.1,
    sigma_rec = 0.1
)
stock$year <- stock$year + 1980

stock$relB <- stock$biomass / (0.3 * stock$Bmsy)
stock$relB_lwr <- exp(log(stock$relB) - 0.2)
stock$relB_upr <- exp(log(stock$relB) + 0.2)
stock$relF <- (stock$catch / stock$biomass) / stock$Fmsy
stock$relF_lwr <- exp(log(stock$relF) - 0.2)
stock$relF_upr <- exp(log(stock$relF) + 0.2)

toy_stock <- stock

library(NAFOdown)
library(ggplot2)
library(patchwork)
showtext::showtext_opts(dpi = 96) # Adjust dpi to match your plotting device (e.g., set to 300 if using ggsave with dpi set to 300)
theme_set(theme_nafo()) # set default ggplot2 theme

C_plot <- ggplot(aes(x = year), data = toy_stock) +
    geom_bar(aes(y = catch, fill = "Catch"), stat = "identity") +
    geom_line(aes(y = TAC, linetype = "TAC")) +
    scale_y_continuous(n.breaks = 10, name = "Catch / TAC", expand = expansion(c(0, 0.05))) +
    scale_x_continuous(n.breaks = 10, name = "", expand = expansion(c(0, 0))) +
    scale_fill_nafo() +
    theme(legend.title = element_blank())

B_plot <- ggplot(aes(x = year), data = toy_stock) +
    geom_ribbon(aes(ymin = relB_lwr, ymax = relB_upr), fill = .nafo_cols[1], alpha = 0.4) +
    geom_line(aes(y = relB), color = .nafo_cols[1]) +
    scale_y_continuous(limits = c(0, NA), n.breaks = 10, name = "B/Blim", expand = expansion(c(0, 0.05))) +
    scale_x_continuous(n.breaks = 10, name = "", expand = expansion(c(0, 0)))

F_plot <- ggplot(aes(x = year), data = toy_stock) +
    geom_ribbon(aes(ymin = relF_lwr, ymax = relF_upr), fill = .nafo_cols[1], alpha = 0.4) +
    geom_line(aes(y = relF), color = .nafo_cols[1]) +
    scale_y_continuous(limits = c(0, NA), n.breaks = 10, name = "F/Flim", expand = expansion(c(0, 0.05))) +
    scale_x_continuous(n.breaks = 10, name = "", expand = expansion(c(0, 0)))

R_plot <- ggplot(aes(x = year), data = toy_stock) +
    geom_line(aes(y = rec_index1, color = "Survey 1")) +
    geom_line(aes(y = rec_index2, color = "Survey 2")) +
    geom_line(aes(y = rec_index3, color = "Survey 3")) +
    scale_y_continuous(limits = c(0, NA), name = "Recruitment index", expand = expansion(c(0, 0.05))) +
    scale_x_continuous(n.breaks = 10, name = "", expand = expansion(c(0, 0))) +
    scale_color_nafo() +
    theme(legend.title = element_blank())

(C_plot + B_plot) / (F_plot + R_plot)


write.csv(toy_stock, file = "data-raw/toy_stock.csv", row.names = FALSE)
usethis::use_data(toy_stock, overwrite = TRUE)
