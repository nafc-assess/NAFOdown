
#' flextable theme for styling tables for NAFO docuemtns
#'
#' @description A modification of [flextable::theme_booktabs]
#'
#' @param x           A flextable object
#' @param fontsize    Font size
#' @param fontname    Font family
#'
#' @export
#'
#' @import flextable
#' @importFrom magrittr "%>%"
#' @importFrom officer fp_border
#'
#' @examples
#'
#' library(flextable)
#' d <- data.frame(SSB = rlnorm(10), Recruitment = rlnorm(10))
#' flextable(d) %>%
#' theme_nafotabs()
#'

theme_nafotabs <- function(x, fontsize = 11, fontname = "Cambria"){

    if ( !inherits(x, "flextable") ) stop("theme_nafotabs supports only flextable objects.")
    big_border <- officer::fp_border(width = 1)
    std_border <- officer::fp_border(width = 0.5)
    h_nrow <- flextable:::nrow_part(x, "header")
    f_nrow <- flextable:::nrow_part(x, "footer")
    b_nrow <- flextable:::nrow_part(x, "body")

    x <- border_remove(x)

    if (h_nrow > 0 ) {
        x <- hline_top(x, border = big_border, part = "header")
        x <- hline(x, border = std_border, part = "header")
        x <- hline_bottom(x, border = big_border, part = "header")
    }
    if (f_nrow > 0 ) {
        # x <- hline(x, border = std_border, part = "footer")
        x <- hline_bottom(x, border = big_border, part = "footer")
    }
    if (b_nrow > 0 ) {
        # x <- hline(x, border = std_border, part = "body")
        x <- hline_bottom(x, border = big_border, part = "body")
    }

    x <- padding(x = x, padding.left = 5, padding.right = 5,
                 padding.bottom = 2, padding.top = 2, part = "all")
    x <- align_text_col(x, align = "left", header = TRUE)
    x <- align_nottext_col(x, align = "right", header = TRUE)
    x <- bg(x = x, bg = "transparent", part = "all")
    x <- font(x, fontname = fontname, part = "all")
    x <- fontsize(x, size = fontsize, part = "all")
    x <- bold(x, part = "header")
    x <- set_formatter_type(x, fmt_double = "%.02f")
    x

}

