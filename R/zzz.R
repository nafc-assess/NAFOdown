
#' @importFrom  sysfonts font_add
#' @import showtext

.onLoad <- function(libname, pkgname) {
    fonts <- list.files(sysfonts::font_paths())
    if ("cambria.ttc" %in% fonts) {
        sysfonts::font_add("Cambria", "cambria.ttc") # font used for NAFO plots
    }
    showtext::showtext_auto()
    showtext::showtext_opts(dpi = 300)
}


.onAttach <- function(libname, pkgname) {
    ## rfiglet startup message
    packageStartupMessage(
        "    _   _____    __________      __\n",
        "   / | / /   |  / ____/ __ \\____/ /___ _      ______\n",
        "  /  |/ / /| | / /_  / / / / __  / __ \\ | /| / / __ \\\n",
        " / /|  / ___ |/ __/ / /_/ / /_/ / /_/ / |/ |/ / / / /\n",
        "/_/ |_/_/  |_/_/    \\____/\\__,_/\\____/|__/|__/_/ /_/",
        " version ", packageVersion(pkgname), "\n\n",
        "Please report issues to https://github.com/nafc-assess/NAFOdown/issues, and\n",
        "send references to documents using NAFOdown to Paul.Regular@dfo-mpo.gc.ca."
        )
}
