

#' Create a new NAFO document based on a template
#'
#' Create a draft of an R Markdown NAFO document
#'
#' @param report_type   The type of report to start.
#' @param create_dir    `TRUE` to create a new directory for the document.
#' @param edit          `TRUE` to edit the template immediately.
#' @param ...           Other arguments to pass to [rmarkdown::draft()].
#'
#' @details This is a light wrapper around [rmarkdown::draft()].
#'          Consult that function for further details.
#'
#' @examples
#' \dontrun{
#' NAFOdown::draft("SCR")
#' }
#' @export
draft <- function(report_type = c("SCR", "STACFIS", "SCS"),
                  create_dir = FALSE,
                  edit = FALSE, ...) {
    report_type <- match.arg(report_type)
    rmarkdown::draft("index.Rmd", template = report_type,
                     package = "NAFOdown", create_dir = create_dir, edit = edit)
}
