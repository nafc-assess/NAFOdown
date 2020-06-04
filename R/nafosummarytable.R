
#' wrapper of flextable for producing summary tables for NAFO documents
#'
#' @description Produces summary tables for NAFO stock summary sheets
#'
#' @param comments A vector of length 5 elaborating on the status the status of each 'Convention General Principle'
#' @param comments A vector of length 5 decribing the status of each 'Convention General Principle'. Must be one of: 'OK', 'Intermediate','Not accomplished', 'Unknown'
#'
#' @export
#'
#' @import flextable officer
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

nafo_summary_table <- function(comments=c(rep(NULL,5)), status=c(rep(NULL,5))){
    if(length(comments)!=5) stop("comments must be a vector of lenght=5 (for each row in table)")
    if(length(status)!=5) stop("status must be a vector of lenght=5 (for each row in table)")
    if(any(status)%in%c('OK', 'Intermediate','Not accomplished', 'Unknown')) stop("status must be one of 'OK', 'Intermediate','Not accomplished', 'Unknown'")
    stock_summary_df <- tibble("Convention General Principles"=c("Restore or maintain at Bmsy","Eliminate overfishing","Apply Percautionary Approach","Minimise harmful impacts on living marine resources and ecosystems","Preserve marine biodiversity"),
                               "Status"=comments,
                               "Comment/consideration"=status)


    green_light <- file.path("images/green-light.jpg")
    yellow_light <- file.path("images/yellow-light.jpg")
    red_light <- file.path("images/red-light.jpg")
    white_light <- file.path("images/white-light.jpg")



    summary_table <- flextable(stock_summary_df) %>%
        align(align="left",part="all") %>%
        autofit() %>%
        compose(j=1,i=1,part="body",
                value= as_paragraph("Restore or maintain at ",as_i("B"),as_i(as_sub("MSY")))) %>%
        compose(j=3,i=1,part="body",
                value= as_paragraph(as_i("B"),as_i(as_sub("MSY")))) %>%
        compose(j=2,i=which(stock_summary_df$Status=="OK"),part="body",
                value= as_paragraph(as_image(src = green_light, width = .20, height = .20))) %>%
        compose(j=2,i=which(stock_summary_df$Status=="Intermediate"),part="body",
                value= as_paragraph(as_image(src = yellow_light, width = .20, height = .20))) %>%
        compose(j=2,i=which(stock_summary_df$Status=="Not accomplished"),part="body",
                value= as_paragraph(as_image(src = red_light, width = .20, height = .20))) %>%
        compose(j=2,i=which(stock_summary_df$Status=="Unknown"),part="body",
                value= as_paragraph(as_image(src = white_light, width = .20, height = .20))) %>%
        compose(j=2,i=which(is.null(stock_summary_df$Status)),part="body",
                value= as_paragraph(as_image(src = white_light, width = .20, height = .20))) %>%
        italic(i=1,part="header")
    summary_Table
}





