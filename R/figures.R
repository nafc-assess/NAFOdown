
#' NAFO plotting theme for ggplot2
#'
#' @param base_size      base font size
#' @param base_family    base font family
#'
#' @seealso [ggplot2::theme_minimal]
#'
#' @description
#'
#' This theme will produce ggplots that follow NAFO plot guidelines:
#' - Font: Cambria 9pt.
#' - Plot Border: black 0.5 pt. No borders around entire graph, just plot. Borders around legend (0.5pt) if possible.
#' - Tick Marks to the outside on x & y axis
#' - Graph lines: 0.75 pt, black if possible, otherwise grey
#' - Plot size: Height 6.4 cm, width 11.5 cm.
#'
#' The helper values [.nafo_lwd], [.nafo_height], and [.nafo_width] can be used to impose consistent graph line widths, plot heights, and plot widths, respectively.
#'
#'
#' @export
#'
#' @import ggplot2
#' @import ggthemes
#' @import scales
#'
#' @example inst/examples/theme_nafo.R
#'

theme_nafo <- function(base_size = 9, base_family = "Cambria"){
    ggthemes::theme_foundation(base_size = base_size, base_family = base_family) %+replace%
        theme(rect = element_rect(color = "black", size = 0.5 / ggplot2::.pt, # 0.5 pt to mm
                                  linetype = "solid", fill = NA),
              line = element_line(colour = "black", size = 0.5 / ggplot2::.pt,
                                  linetype = "solid", lineend = "butt"),
              axis.ticks = element_line(colour = "black"),
              axis.ticks.length = unit(0.15, "cm"),
              axis.text.x = element_text(margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")),
              axis.text.y = element_text(margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), hjust = 1),
              axis.line = element_line(colour = "black"),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(colour = "lightgrey"),
              panel.background = element_rect(colour = "black"),
              panel.spacing = unit(0.5, unit = "cm"),
              plot.background = element_rect(color = NA, fill = "white"),
              plot.margin = margin(l = 0.2, r = 0.2, b = 0.2, t = 0.2, unit = "cm"),
              strip.background = element_blank(),
              legend.key = element_blank(),
              legend.background = element_blank(),
              legend.box.background = element_rect(color = "black", size = 0.5 / ggplot2::.pt,
                                                   linetype = "solid", fill = "white"),
              legend.title = element_text(size = base_size - 2, hjust = 0),
              legend.justification = c(1, 1),
              legend.position = c(1, 1),
              legend.key.size = unit(1, "line"),
              legend.margin = margin(l = 1, r = 1, b = 0.1, t = 0.1, unit = "mm"),
              legend.box.margin = margin(l = 1, r = 1, b = 0.1, t = 0.1, unit = "mm"))
}


#' NAFO color palette for ggplot2
#'
#' These functions provide custom color and fill scales for ggplot2 plots, based on a NAFO color palette.
#'
#' @param type Either "continuous" or "discrete" to specify the scale type.
#' @inheritParams nafo_pal
#' @param ... Additional arguments passed to ggplot scale functions.
#'
#' @return A ggplot2 scale function.
#' @export
#'
#' @example inst/examples/scale_colour_nafo.R
#'
#' @rdname scale_nafo
scale_colour_nafo <- function(type = "discrete", palette = "logo", ...) {
    pal <- nafo_pal(palette)
    if (type == "continuous") {
        f <- ggplot2::continuous_scale("colour", "nafo", palette = scales::gradient_n_pal(pal), ...)
    } else if (type == "discrete") {
        f <- ggplot2::discrete_scale("colour", "nafo", scales::manual_pal(values = pal), ...)
    } else {
        stop("Unknown scale type. Use 'continuous' or 'discrete'.")
    }
    f
}

#' @rdname scale_nafo
#' @export
scale_color_nafo <- scale_colour_nafo

#' @rdname scale_nafo
#' @export
scale_fill_nafo <- function(type = "discrete", palette = "logo", ...) {
    pal <- nafo_pal(palette)
    if (type == "continuous") {
        f <- ggplot2::continuous_scale("fill", "nafo", palette = scales::gradient_n_pal(pal), ...)
    } else if (type == "discrete") {
        f <- ggplot2::discrete_scale("fill", "nafo", scales::manual_pal(values = pal), ...)
    } else {
        stop("Unknown scale type. Use 'continuous' or 'discrete'.")
    }
    f
}

## Simple function for making a colour lighter
lighten_colour <- function(col, percent) {
    colorRampPalette(c(col, "#ffffff"))(100)[percent]
}

#' NAFO Colour Palettes
#'
#' This function returns a colour palette function based on the specified type.
#'
#' @param palette Type of palette. Options are "logo", "blues", "greens", "blue2green", and "hot_cold".
#' @param n Number of colours to return.
#'
#' @return A function that generates the specified palette.
#' @export
#'
#' @examples
#' nafo_pal("logo")
#' nafo_pal("blues", 10)
#' nafo_pal("hot_cold", 10)
#'
nafo_pal <- function(palette = "logo", n = length(.nafo_cols)) {

    values <- switch(
        palette,
        "logo" = .nafo_cols,
        "blues" = c(lighten_colour(.nafo_cols[1], 80), .nafo_cols[1]),
        "greens" = c(lighten_colour(.nafo_cols[2], 80), .nafo_cols[2]),
        "blue2green" = c(.nafo_cols[3], lighten_colour(.nafo_cols[1], 50), lighten_colour(.nafo_cols[2], 70)),
        "hot_cold" = c(.nafo_cols[5], "#ffffff", .nafo_cols[1]),
        stop("Unknown palette type. Available options are 'logo', 'blues', 'greens', 'blue2green', and 'hot_cold'.")
    )

    if (palette == "logo" && n > length(values)) {
        stop("Requested more colors than are available in the qualitative palette.")
    }

    colorRampPalette(values)(n)
}


#' Helper values for consistent plot settings
#'
#' @description
#' - `.nafo_pts`: this value is to be used in geom_point to define point size (it is 3 pt converted to mm)
#' - `.nafo_lwd`: this value is to be used in geom_line to define line width (it is 0.75 pt converted to mm)
#' - `.nafo_stroke`: this value is to be used in geom_point to define stroke width (it is 0.75 pt converted to 1/96 inches)
#' - `.nafo_height`: this value is to be used to define plot height in inches (it is 6.4 cm converted to inches)
#' - `.nafo_width`: this value is to be used to define plot width in inches (it is 11.5 cm converted to inches)
#' - `.nafo_asp`: this value is to be used to define the aspect ratio of a plot
#' - `.nafo_cols`: vector of colours to use in plots
#'
#' @name nafo-vals
NULL

#' @export
#' @rdname nafo-vals
.nafo_pts <- 3 / ggplot2::.pt

#' @export
#' @rdname nafo-vals
.nafo_lwd <- 0.75 / ggplot2::.pt

#' @export
#' @rdname nafo-vals
.nafo_stroke <- .nafo_lwd * ggplot2::.stroke / 2

#' @export
#' @rdname nafo-vals
.nafo_height <- 6.4 / 2.54

#' @export
#' @rdname nafo-vals
.nafo_width <- 11.5 / 2.54

#' @export
#' @rdname nafo-vals
.nafo_asp <- 6.4 / 11.5

#' @export
#' @rdname nafo-vals
.nafo_cols <- c("#0079C0", "#00A64F", "#2B3991", "#00CED1", "#FF4500", "#FFD700", "#767676")


#' Generate ridges plot from annual length frequency data
#'
#' @description Function for producing a faceted ridge plot of annual length frequencies using the ggridges package.
#'
#' @param data           A data.frame with `"year"`, `"length"`, and `"frequency"` headers. Extra columns
#'                       may aso be utilized to define facets using the rows and cols arguments.
#' @param rows           Column to use to define facet rows.
#' @param cols           Column to use to define facet columns.
#' @param xlab           Label for x-axis.
#' @param ylab           Label for y-axis.
#' @param scale          Scaling factor controlling the height of the ridges.
#' @param alpha          Value controling the transparancy of the ridges.
#' @param scale_color    Function such as [ggplot2::scale_colour_viridis_d] for defining line
#'                       colours to use across years.
#' @param scale_fill     Function such as [ggplot2::scale_fill_viridis_d] for defining fill
#'                       colours to use across years.
#' @param drop_zeros     Should zeros be replaced by NA values? (i.e., impose blank space in ridge plot).
#'
#' @return Returns a ggplot2 object that can be modified further by functions such as [ggplot2::theme()].
#'
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' ## Simulate some data
#' set.seed(3)
#' samples_per_year <- 1000
#' years <- 2000:2022
#' lambdas <- rnorm(length(years), mean = 25, sd = 10)
#' data <- lapply(seq_along(years), function(i) {
#'     lengths <- rpois(samples_per_year, lambda = lambdas[i])
#'     lf <- table(lengths)
#'     data.frame(year = years[i], length = as.numeric(names(lf)), frequency = as.numeric(unname(lf)))
#' })
#' data <- do.call(rbind, data)
#'
#' ## Using defaults
#' plot_lengths(data)
#'
#' ## Customize using ggplot functions
#' plot_lengths(data, alpha = 0.1, scale = 2) +
#' scale_color_manual(values = rep("grey30", length(unique(data$year)))) +
#' scale_fill_manual(values = rep("grey30", length(unique(data$year))))
#'
#'
#' @import ggridges
#'
plot_lengths <- function(data, rows = NULL, cols = NULL,
                         xlab = "Length (cm)", ylab = "Year",
                         scale = 5, alpha = 0.8,
                         scale_color = scale_color_viridis_d,
                         scale_fill = scale_fill_viridis_d,
                         drop_zeros = TRUE) {

    if (drop_zeros) data$freq[data$freq == 0] <- NA

    ggplot(data)  +
        theme_ridges(grid = TRUE, center_axis_labels = TRUE, font_size = 3.5) +
        geom_density_ridges(aes(x = as.numeric(length), y = as.factor(year),
                                height = as.numeric(frequency), fill = as.factor(year),
                                color = as.factor(year)),
                            stat = "identity", scale = scale, alpha = alpha, show.legend = FALSE) +
        facet_grid(rows = rows, cols = cols, scales = "free_y", space = "free_y") +
        scale_color() + scale_fill() +
        labs(x = xlab, y = ylab)+
        theme_nafo()+
        theme(panel.background = element_blank(),
              panel.grid = element_blank(),
              panel.spacing.x = unit(0,"line"))

}

