

#' Create a new NAFO document based on a template
#'
#' Create a draft of an R Markdown NAFO document
#'
#' @param type The type of document to start.
#' @param create_dir `TRUE` to create a new directory for the document.
#' @param edit `TRUE` to edit the template immediately.
#' @param ... Other arguments to pass to [rmarkdown::draft()].
#'
#' @details This is a light wrapper around [rmarkdown::draft()]. Consult that
#'   function for further details.
#'
#' @examples
#' \dontrun{
#' NAFOdown::draft("SCR")
#' }
#' @export
draft <- function(type = c("SCR"),
                  create_dir = FALSE,
                  edit = FALSE, ...) {
    type <- match.arg(type)
    rmarkdown::draft("index.Rmd", template = type,
                     package = "NAFOdown", create_dir = create_dir, edit = edit)
}
