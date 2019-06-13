
#' ggplot2 theme for styling plots for NAFO docuemtns
#'
#' @param base_size      base font size
#' @param base_family    base font family
#'
#' @seealso [ggplot2::theme_minimal]
#'
#' @export
#'
#' @import ggplot2
#' @import ggthemes
#'
#' @examples
#'
#' d <- data.frame(SSB = rlnorm(20), Recruitment = rlnorm(20))
#' ggplot(d) +
#' geom_point(aes(x = SSB, y = Recruitment)) +
#' theme_nafo()
#'

theme_nafo <- function(base_size = 14, base_family = "serif"){
    theme_foundation(base_size = base_size, base_family = base_family) %+replace%
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_blank(),
              strip.background = element_blank(),
              legend.key = element_blank(),
              legend.background = element_blank(),
              legend.box.background = element_blank(),
              legend.title = element_blank(),
              legend.justification = c(1, 1),
              legend.position = c(1, 1),
              legend.key.size = unit(1, "line"))
}
