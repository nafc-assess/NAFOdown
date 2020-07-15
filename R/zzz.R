

#' @importFrom  sysfonts font_add
#' @import showtext

.onLoad <- function(libname, pkgname){
    sysfonts::font_add("Cambria", "cambria.ttc") # font used for NAFO plots
    showtext::showtext_auto()
}
