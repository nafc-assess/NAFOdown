
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
    q1 = 0.5,
    q2 = 0.4,
    q3 = 0.3,
    a = 5,
    b = 0.001,
    years = 40,
    TAC_interval = 5,
    sigma_biomass = 0.1,
    sigma_TAC = 0.2,
    sigma_catch = 0.0005,
    sigma_index = 0.05,
    sigma_rec = 0.2
)
stock$year <- stock$year + 1980

stock$relB <- stock$biomass / (stock$Bmsy)
stock$relB_lwr <- exp(log(stock$relB) - 0.2)
stock$relB_upr <- exp(log(stock$relB) + 0.2)
stock$relF <- (stock$catch / stock$biomass) / stock$Fmsy
stock$relF_lwr <- exp(log(stock$relF) - 0.2)
stock$relF_upr <- exp(log(stock$relF) + 0.2)

plot(biomass ~ year, data = stock, type = "l", xlab = "Year", ylab = "Biomass")
lines(stock$year, stock$biomass_lwr, lty = 3)
lines(stock$year, stock$biomass_upr, lty = 3)
plot(TAC ~ year, data = stock, type = "l", xlab = "Year", ylab = "Catch/TAC")
points(stock$year, stock$catch)
plot(relF ~ year, data = stock, type = "l", xlab = "Year", ylab = "F/Fmsy")
plot(rec_index1 ~ year, data = stock, type = "l", xlab = "Year", ylab = "Recruitment index")
lines(stock$year, stock$rec_index2, lty = 2)
lines(stock$year, stock$rec_index3, lty = 3)

toy_stock <- stock
usethis::use_data(toy_stock, overwrite = TRUE)
