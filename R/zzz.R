

#' @importFrom  sysfonts font_add
#' @import showtext

.onLoad <- function(libname, pkgname){
    fonts <- list.files(sysfonts::font_paths())
    if ("cambria.ttc" %in% fonts) {
        sysfonts::font_add("Cambria", "cambria.ttc") # font used for NAFO plots
    }
    showtext::showtext_auto()
}
