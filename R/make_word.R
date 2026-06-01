

.scs_title_tab_openxml <- "`<w:r><w:tab/></w:r>`{=openxml}"

.inject_scs_title_tabs <- function(lines) {
    in_title_div <- FALSE

    vapply(lines, function(line) {
        if (grepl('^:::\\s*\\{.*custom-style\\s*=\\s*["\']Title["\']', line)) {
            in_title_div <<- TRUE
            return(line)
        }

        if (in_title_div && grepl('^:::\\s*$', line)) {
            in_title_div <<- FALSE
            return(line)
        }

        if (in_title_div && grepl(" Advice", line, fixed = TRUE) &&
            !grepl("<w:tab\\s*/>|\\tAdvice", line, perl = TRUE)) {
            line <- sub(" Advice", paste0(.scs_title_tab_openxml, "Advice"),
                        line, fixed = TRUE)
        }

        line
    }, character(1), USE.NAMES = FALSE)
}

.inject_scs_title_tabs_file <- function(input_file) {
    if (!file.exists(input_file)) {
        return(invisible(FALSE))
    }

    lines <- readLines(input_file, warn = FALSE)
    updated_lines <- .inject_scs_title_tabs(lines)

    if (!identical(lines, updated_lines)) {
        writeLines(updated_lines, input_file, useBytes = TRUE)
        return(invisible(TRUE))
    }

    invisible(FALSE)
}

.add_scs_title_tab_pre_processor <- function(format) {
    pre_processor <- format$pre_processor

    format$pre_processor <- function(metadata, input_file, runtime, knit_meta,
                                     files_dir, output_dir) {
        .inject_scs_title_tabs_file(input_file)

        if (is.function(pre_processor)) {
            pre_processor(metadata, input_file, runtime, knit_meta, files_dir,
                          output_dir)
        }
    }

    format
}

.make_word_function <- function(format_function, reference_docx) {
    function(...) {
        base <- format_function(...,
                                number_sections = FALSE,
                                reference_docx = system.file("docx", reference_docx, package = "NAFOdown")
        )
        base$knitr$opts_chunk$comment <- NA
        .add_scs_title_tab_pre_processor(base)
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
