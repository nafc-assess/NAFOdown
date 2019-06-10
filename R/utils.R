
#' Creates an R Markdown Word NAFO-formatted document
#'
#' This is a function called in output in the YAML of the driver Rmd file
#' to specify the creation of a Microsoft Word version of an SCR or a
#' STACFIC report.
#'
#' @param ... arguments to pass to [bookdown::word_document2()]
#'
#' @return A Word Document based on the NAFO SCR word template.
#'
#' @import bookdown
#' @rdname word_scr
#' @export
#'

word_scr <- function(...) {

    base <- word_document2(...,
        reference_docx = system.file("docx", "SCR_template.docx", package = "NAFOdown")
    )

    # Mostly copied from knitr::render_sweave
    base$knitr$opts_chunk$comment <- NA
    base$knitr$opts_chunk$fig.align <- "center"
    base

}


#' @rdname word_scr
#' @export
#'
word_stacfis <- function(...) {

    base <- word_document2(...,
                           reference_docx = system.file("docx", "STACFIS_template.docx", package = "NAFOdown")
    )

    # Mostly copied from knitr::render_sweave
    base$knitr$opts_chunk$comment <- NA
    base$knitr$opts_chunk$fig.align <- "center"
    base

}
