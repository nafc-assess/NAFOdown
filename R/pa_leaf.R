
#' Calculate fishing mortality under the PA leaf
#'
#' These functions calculate fishing mortality based on biomass using different harvest control rule (HCR) approaches:
#'
#' - `F_linear()` implements a linear HCR
#' - `F_sigmoid()` implements a sigmoid HCR with configurable inflection point (x50)
#' - `F_lower()` sets x50 = 0.75 for the lower bound of the leaf as per Annex 4 in [Com-SC RBMS WP 23-03 Rev 2](https://www.nafo.int/Portals/0/PDFs/COM-SC/2023/com-scdoc23-03.pdf)
#' - `F_upper()` sets x50 = 0.25 for the upper bound of the leaf as per Annex 4 in [Com-SC RBMS WP 23-03 Rev 2](https://www.nafo.int/Portals/0/PDFs/COM-SC/2023/com-scdoc23-03.pdf)
#'
#' @param B Biomass vector
#' @param Blim Biomass limit reference point
#' @param Btrigger Biomass trigger reference point
#' @param Ftarget Target fishing mortality
#' @param Fmin Minimum fishing mortality (default: 0)
#' @param x50 (for `F_sigmoid`) Proportion of the biomass range at which F = 0.5 * Ftarget
#' @param k (for `F_sigmoid`) Shape parameter for the sigmoid function (default: 1)
#'
#' @return Numeric vector of fishing mortality values
#' @export
#' @rdname F_linear
F_linear <- function(B, Blim, Btrigger, Ftarget, Fmin = 0) {
    slope <- Ftarget / (Btrigger - Blim)
    intercept <- -slope * Blim
    F_t <- ifelse(
        B <= Blim, Fmin,
        ifelse(B >= Btrigger, Ftarget, slope * B + intercept)
    )
    return(F_t)
}

#' @export
#' @rdname F_linear
F_sigmoid <- function(B, Blim, Btrigger, Ftarget, x50, k = 1, Fmin = 0) {
    B_range <- Btrigger - Blim
    B_50 <- x50 * B_range + Blim
    aBx <- (B_50 - Blim)^k
    bBx <- (Btrigger - Blim)^k
    Bx <- Blim + ((aBx * bBx) / (bBx - 2 * aBx))^(1 / k)
    cA <- (Bx - Blim)^k
    A <- (Ftarget * (cA + bBx)) / bBx
    dBt <- (B - Blim)^k
    Ft_mid <- (A * dBt) / (cA + dBt)

    F_t <- ifelse(
        B <= Blim, Fmin,
        ifelse(B >= Btrigger, Ftarget, Ft_mid)
    )
    return(F_t)
}

#' @export
#' @rdname F_linear
F_lower <- function(B, Blim, Btrigger, Ftarget, k = 1, Fmin = 0) {
    F_sigmoid(B = B, Blim = Blim, Btrigger = Btrigger, Ftarget = Ftarget,
              x50 = 0.75, k = k, Fmin = Fmin)
}

#' @export
#' @rdname F_linear
F_upper <- function(B, Blim, Btrigger, Ftarget, k = 1, Fmin = 0) {
    F_sigmoid(B = B, Blim = Blim, Btrigger = Btrigger, Ftarget = Ftarget,
              x50 = 0.25, k = k, Fmin = Fmin)
}


#' Prepare data for plotting the Precautionary Approach (PA) Leaf
#'
#' This function processes raw input and calculates the key elements needed to plot the PA leaf:
#' the HCR bounds (linear, upper, lower), axis limits, and biomass sequence for plotting ribbons.
#' Used internally by `plot_PA_leaf()` but can also be called directly to inspect data.
#'
#' @param data A data frame with the following required columns:
#'   - `year`: numeric year values
#'   - `Btrend`: numeric biomass values
#'   - `Ftrend`: numeric fishing mortality values
#'
#' Optional columns:
#'   - `Blwr`, `Bupr`: biomass uncertainty bounds
#'   - `Flwr`, `Fupr`: F uncertainty bounds
#'   - `scenario`: projection scenario for linetype distinction and legend
#'   - `label_just`: justification for year label (`"left"`, `"center"`, `"right"`)
#'
#' @inheritParams F_linear
#' @param Flim Fishing mortality limit
#' @param zone_labels PA zone labels. Defaults may need to be modified to keep
#' the text inside the zone (e.g., supply `"Cautious"` rather than `"Cautious Zone"`).
#' An empty character may also be supplied if the zone is too narrow to label.
#'
#' @seealso [plot_PA_leaf], [F_linear], [F_upper], [F_lower], [F_sigmoid]
#'
#' @return A list with components: `data`, `Bseq`, `leaf_linear`, `leaf_low`, `leaf_high`, `xhigh`, `yhigh`
#'
#' @export
make_PA_data <- function(data, Blim, Btrigger, Ftarget, Flim,
                         zone_labels = c("Critical Zone", "Cautious Zone", "Healthy Zone")) {
    if (is.null(data)) {
        data <- data.frame(year = -1, Btrend = -1, Ftrend = -1)
    }
    if (is.null(data$Blwr)) data$Blwr <- -1
    if (is.null(data$Bupr)) data$Bupr <- -1
    if (is.null(data$Flwr)) data$Flwr <- -1
    if (is.null(data$Fupr)) data$Fupr <- -1
    if (is.null(data$label_just)) data$label_just <- "center"
    if (is.null(data$scenario)) {
        data$scenario <- NA
    } else {
        if (length(unique(data$scenario)) > 3) {
            stop("The PA plot was not built to display more than three projection scenarios.")
        }
    }

    xhigh <- max(max(data$Btrend, na.rm = TRUE) * 1.2,
                 max(data$Bupr, na.rm = TRUE) * 1.2,
                 Btrigger * 1.5)
    yhigh <- max(max(data$Ftrend, na.rm = TRUE) * 1.2,
                 max(data$Fupr, na.rm = TRUE) * 1.2,
                 Flim * 1.5)

    Bseq <- seq(Blim, Btrigger, length.out = 100)
    leaf_linear <- F_linear(Bseq, Blim, Btrigger, Ftarget)
    leaf_low <- F_lower(Bseq, Blim, Btrigger, Ftarget)
    leaf_high <- F_upper(Bseq, Blim, Btrigger, Ftarget)

    as.list(environment())
}

#' Plot the Precautionary Approach (PA) Leaf with Historical or Projection Scenario Data
#'
#' Generates a stylized Precautionary Approach (PA) "leaf" plot using the NAFO Precautionary Approach Framework (COM Doc. 24-25).
#' The plot displays background management zones (Critical, Cautious, Healthy), HCR curves, and
#' overlays of biomass and fishing mortality trends or projections across years.
#'
#' @inheritParams make_PA_data
#'
#' @return A `ggplot` object showing the PA leaf, and biomass-F trajectories.
#'
#' @example inst/examples/make_pa_leaf.R
#'
#' @seealso [make_PA_data], [F_linear], [F_upper], [F_lower], [F_sigmoid]
#'
#' @import ggrepel
#'
#' @export
plot_PA_leaf <- function(data, Blim, Btrigger, Ftarget, Flim,
                         zone_labels = c("Critical Zone", "Cautious Zone", "Healthy Zone")) {

    PA_data <- make_PA_data(data, Blim, Btrigger, Ftarget, Flim,
                            zone_labels = zone_labels)

    with(PA_data, {
        ggplot() +
            geom_rect(aes(xmin = 0, xmax = Blim, ymin = 0, ymax = yhigh), fill = "indianred", alpha = 0.4) +
            geom_rect(aes(xmin = Blim, xmax = Btrigger, ymin = 0, ymax = yhigh), fill = "khaki", alpha = 0.4) +
            geom_rect(aes(xmin = Btrigger, xmax = xhigh, ymin = 0, ymax = yhigh), fill = "palegreen", alpha = 0.4) +

            geom_ribbon(aes(x = Bseq, ymin = leaf_low, ymax = leaf_high), fill = "ivory", linewidth = .nafo_lwd) +
            geom_line(aes(x = Bseq, y = leaf_linear), color = "grey75", linewidth = .nafo_lwd) +
            geom_line(aes(x = Bseq, y = leaf_low), color = "grey75", linewidth = .nafo_lwd) +
            geom_line(aes(x = Bseq, y = leaf_high), color = "grey75", linewidth = .nafo_lwd) +

            geom_hline(aes(yintercept = Flim), color = "grey75", linewidth = .nafo_lwd) +
            geom_hline(aes(yintercept = Ftarget), linetype = 2, color = "grey75", linewidth = .nafo_lwd) +
            geom_vline(aes(xintercept = Blim), color = "indianred", , linewidth = .nafo_lwd * 1.5) +
            geom_vline(aes(xintercept = Btrigger), color = "khaki", , linewidth = .nafo_lwd * 1.5) +

            geom_text(aes(x = c(Blim / 2, mean(c(Blim, Btrigger)), mean(c(Btrigger, xhigh))),
                          y = I(rep(0.97, 3)), label = zone_labels),
                      vjust = 1, size = 3, family = "Cambria") +

            geom_text_repel(aes(x = c(Blim, Btrigger), y = I(rep(0.97, 2)),
                                label = c("B[lim]~~~phantom('x')", "B[trigger]~~~phantom('x')")),
                            vjust = -0.2, hjust = "right", size = 3, angle = 90,
                            parse = TRUE, family = "Cambria",
                            min.segment.length = Inf, direction = "y") +
            geom_text_repel(aes(x = I(rep(0.99, 2)), y = c(Ftarget,  Flim),
                                label = c("F[target]",  "F[lim]")),
                            vjust = 0, hjust = "right", size = 3,
                            parse = TRUE, family = "Cambria",
                            min.segment.length = Inf, direction = "y") +

            geom_path(data = data[is.na(data$scenario), ], aes(x = Btrend, y = Ftrend),
                      color = "grey40", linewidth = .nafo_lwd) +
            geom_point(data = data[is.na(data$scenario), ], aes(x = Btrend, y = Ftrend),
                       color = "grey40", size = .nafo_pts * 0.5) +
            geom_path(data = data[!is.na(data$scenario), ], aes(x = Btrend, y = Ftrend, linetype = scenario),
                      color = "black", linewidth = .nafo_lwd) +
            geom_point(data = data[!is.na(data$scenario), ], aes(x = Btrend, y = Ftrend),
                       color = "black", size = .nafo_pts * 0.5) +
            geom_path(data = data[!is.na(data$scenario) & data$year <= (min(data$year) + 1), ],
                      aes(x = Btrend, y = Ftrend),
                      color = "black", linewidth = .nafo_lwd * 0.5) +

            ggrepel::geom_text_repel(data = head(data[data$year == min(data$year), ], 1),
                                     aes(x = Btrend, y = Ftrend, label = unique(year),
                                         hjust = label_just),
                                     size = 2) +
            ggrepel::geom_text_repel(data = data[data$year == max(data$year), ],
                                     aes(x = Btrend, y = Ftrend, label = unique(year),
                                         hjust = label_just),
                                     size = 2) +

            geom_segment(data = data[data$year == max(data$year), ],
                         aes(x = Blwr, y = Ftrend, xend = Bupr, yend = Ftrend), linewidth = .nafo_lwd * 1.5) +
            geom_segment(data = data[data$year == max(data$year), ],
                         aes(x = Btrend, y = Flwr, xend = Btrend, yend = Fupr), linewidth = .nafo_lwd * 1.5) +

            labs(x = "Biomass", y = "Fishing mortality") +
            scale_x_continuous(limits = c(0, xhigh), expand = c(0, 0)) +
            scale_y_continuous(limits = c(0, yhigh), expand = c(0, 0)) +
            scale_linetype_manual(values = c(5, 2, 3)) +
            NAFOdown::theme_nafo() +
            theme(
                plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
                plot.background = element_rect(fill = "white", colour = "white"),
                legend.position.inside = c(0.95, 0.05),
                legend.justification = c(1, 0),
                legend.title = element_blank(),
                legend.box.background = element_rect(fill = "white")
            )
    })

}
