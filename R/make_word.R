
.make_word_function <- function(format_function, reference_docx) {
    function(...) {
        base <- format_function(...,
                                number_sections = FALSE,
                                reference_docx = system.file("docx", reference_docx, package = "NAFOdown")
        )
        base$knitr$opts_chunk$comment <- NA
        base
    }
}


#' Creates an R Markdown Word NAFO-formatted document
#'
#' This is a function called in output in the YAML of the driver Rmd file
#' to specify the creation of a Microsoft Word version of an SCR, STACFIC
#' or SCS report. Functions with 2 as a suffix are built using
#' [officedown::rdocx_document()], allowing users to leverage more in depth
#' customization of Word output.
#'
#' @param ... arguments to pass to [bookdown::word_document2()] or [officedown::rdocx_document()]
#'            for functions with 2 as a suffix.
#'
#' @return A Word Document based on the NAFO SCR, STACFIC or SCS word template.
#'
#' @import bookdown
#' @import officedown
#' @rdname word_scr
#' @export
word_scr <- .make_word_function(bookdown::word_document2, "SCR_template.docx")

#' @rdname word_scr
#' @export
word_scr2 <- .make_word_function(officedown::rdocx_document, "SCR_template.docx")

#' @rdname word_scr
#' @export
word_stacfis <- .make_word_function(bookdown::word_document2, "STACFIS_template.docx")

#' @rdname word_scr
#' @export
word_stacfis2 <- .make_word_function(officedown::rdocx_document, "STACFIS_template.docx")

#' @rdname word_scr
#' @export
word_scs <- .make_word_function(bookdown::word_document2, "SCS_template.docx")

#' @rdname word_scr
#' @export
word_scs2 <- .make_word_function(officedown::rdocx_document, "SCS_template.docx")
