
#' ggplot2 theme for styling plots for NAFO docuemtns
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
#' - Tick Marks to the inside on x & y axis
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
#'
#' @examples
#'
#' library(ggplot2)
#' d <- data.frame(SSB = rlnorm(20), Recruitment = rlnorm(20))
#' ggplot(d) +
#' geom_point(aes(x = SSB, y = Recruitment)) +
#' theme_nafo()
#'

theme_nafo <- function(base_size = 9, base_family = "Cambria"){
    ggthemes::theme_foundation(base_size = base_size, base_family = base_family) %+replace%
        theme(rect = element_rect(color = "black", size = 0.5 / ggplot2::.pt, # 0.5 pt to mm
                                  linetype = "solid", fill = NA),
              line = element_line(colour = "black", size = 0.5 / ggplot2::.pt,
                                  linetype = "solid", lineend = "butt"),
              axis.ticks.length = unit(-0.15, "cm"),
              axis.text.x = element_text(margin = unit(c(0.25, 0.1, 0.1, 0.1), "cm")),
              axis.text.y = element_text(margin = unit(c(0.1, 0.25, 0.1, 0.1), "cm"), hjust = 1),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_blank(),
              strip.background = element_blank(),
              legend.key = element_blank(),
              legend.background = element_blank(),
              legend.box.background = element_rect(color = "black", size = 0.5 / ggplot2::.pt, # 0.5 pt to mm
                                                   linetype = "solid", fill = NA),
              legend.title = element_text(size = base_size - 2, hjust = 0),
              legend.justification = c(1, 1),
              legend.position = c(0.98, 0.98),
              legend.key.size = unit(1, "line"),
              legend.margin = margin(l = 1, r = 1, b = 0.5, unit = "mm"),
              legend.box.margin = margin(l = 1, r = 1, b = 0.5, unit = "mm"))
}



#' Helper values for consistent plot settings
#'
#' @description
#' - `.nafo_lwd`: this value is to be used in geom_line to define line width (it is 0.75 pt converted to mm)
#' - `.nafo_height`: this value is to be used to define plot height in inches (it is 6.4 cm converted to inches)
#' - `.nafo_width`: this value is to be used to define plot width in inches (it is 11.5 cm converted to inches)
#' - `.nafo_asp`: this value is to be used to define the aspect ratio of a plot
#'
#' @name nafo-vals
NULL

#' @export
#' @rdname nafo-vals
.nafo_lwd <- 0.75 / ggplot2::.pt

#' @export
#' @rdname nafo-vals
.nafo_height <- 6.4 / 2.54

#' @export
#' @rdname nafo-vals
.nafo_width <- 11.5 / 2.54

#' @export
#' @rdname nafo-vals
.nafo_asp <- 6.4 / 11.5

