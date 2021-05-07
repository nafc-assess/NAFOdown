
#' Creates an R Markdown Word NAFO-formatted document
#'
#' This is a function called in output in the YAML of the driver Rmd file
#' to specify the creation of a Microsoft Word version of an SCR, STACFIC
#' or SCS report.
#'
#' @param ... arguments to pass to [bookdown::word_document2()]
#'
#' @return A Word Document based on the NAFO SCR, STACFIC or SCS word template.
#'
#' @import officedown
#' @import officer
#' @rdname word_scr
#' @export
#'
word_scr <- function(...) {
    base <- officedown::rdocx_document(
        base_format = "bookdown::word_document2",
        number_sections = FALSE,
        reference_docx = system.file("docx", "SCR_template.docx", package = "NAFOdown"),
        page_margins = officer::page_mar(),
        page_size = officer::page_size(width = 8.5, height = 11),
        ...
    )
    base$knitr$opts_chunk$comment <- NA
    base
}


#' @rdname word_scr
#' @export
#'
word_stacfis <- function(...) {
    base <- officedown::rdocx_document(
        base_format = "bookdown::word_document2",
        number_sections = FALSE,
        reference_docx = system.file("docx", "STACFIS_template.docx", package = "NAFOdown"),
        ...
    )
    base$knitr$opts_chunk$comment <- NA
    base
}

#' @rdname word_scr
#' @export
#'
word_scs <- function(...) {
    base <- officedown::rdocx_document(
        base_format = "bookdown::word_document2",
        number_sections = FALSE,
        reference_docx = system.file("docx", "SCS_template.docx", package = "NAFOdown"),
        ...
    )
    base$knitr$opts_chunk$comment <- NA
    base
}
