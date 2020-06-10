
#' flextable theme for styling tables for NAFO docuemtns
#'
#' @description A modification of [flextable::theme_booktabs]
#'
#' @param x           A flextable object
#' @param fontsize    Font size
#' @param fontname    Font family
#'
#' @export
#'
#' @import flextable
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

theme_nafotabs <- function(x, fontsize = 11, fontname = "Cambria"){

    if ( !inherits(x, "flextable") ) stop("theme_nafotabs supports only flextable objects.")
    big_border <- officer::fp_border(width = 1)
    std_border <- officer::fp_border(width = 0.5)
    h_nrow <- flextable:::nrow_part(x, "header")
    f_nrow <- flextable:::nrow_part(x, "footer")
    b_nrow <- flextable:::nrow_part(x, "body")

    x <- border_remove(x)

    if (h_nrow > 0 ) {
        x <- hline_top(x, border = big_border, part = "header")
        x <- hline(x, border = std_border, part = "header")
        x <- hline_bottom(x, border = big_border, part = "header")
    }
    if (f_nrow > 0 ) {
        # x <- hline(x, border = std_border, part = "footer")
        x <- hline_bottom(x, border = big_border, part = "footer")
    }
    if (b_nrow > 0 ) {
        # x <- hline(x, border = std_border, part = "body")
        x <- hline_bottom(x, border = big_border, part = "body")
    }

    x <- padding(x = x, padding.left = 5, padding.right = 5,
                 padding.bottom = 2, padding.top = 2, part = "all")
    x <- align_text_col(x, align = "left", header = TRUE)
    x <- align_nottext_col(x, align = "right", header = TRUE)
    x <- bg(x = x, bg = "transparent", part = "all")
    x <- font(x, fontname = fontname, part = "all")
    x <- fontsize(x, size = fontsize, part = "all")
    x <- bold(x, part = "header")
    x <- set_formatter_type(x, fmt_double = "%.02f")
    x

}



## little helper to create a paragraph for flextable with content formated
## using markdown like syntax (..but definitly not as fully featured or fool proof)
.as_markdown_like <- function(txt) {

    ## add spaces around special characters
    txt <- gsub("\\*\\*", " __ ", txt) # bold
    txt <- gsub("\\*", " \\* ", txt) # italics
    txt <- gsub("\\~", " \\~ ", txt) # subscript
    txt <- gsub("\\^", " \\^ ", txt) # superscript

    ## split text at spaces
    txt_split <- unlist(strsplit(txt, "\\s+"))
    txt_as_i <- cumsum(txt_split == "*") %% 2
    txt_as_bold <- cumsum(txt_split == "__") %% 2
    txt_as_sub <- cumsum(txt_split == "~") %% 2
    txt_as_sup <- cumsum(txt_split == "^") %% 2

    ## set-up a paragraph and assign formatting
    para <- as_paragraph(list_values = as.list(txt_split))
    p <- para[[1]]
    p$italic <- as.logical(txt_as_i)
    p$bold <- as.logical(txt_as_bold)
    p$vertical.align[as.logical(txt_as_sub)] <- "subscript"
    p$vertical.align[as.logical(txt_as_sup)] <- "superscript"
    p <- p[!(p$txt %in% c("*", "__", "~", "^")), ] # drop special characters used to define formatting
    p$txt <- paste0(p$txt, " ")
    p$seq_index <- seq(nrow(p))
    para[[1]] <- p
    para

}


#' Function for producing summary tables for NAFO documents
#'
#' @description Produces summary tables for NAFO stock summary sheets
#'
#' @param comments  A vector of length 5 elaborating on the status the status of each 'Convention General Principle'
#' @param status    A vector of length 5 decribing the status of each 'Convention General Principle'. Must be one of: 'OK', 'Intermediate','Not accomplished', 'Unknown'
#'
#' @export
#'
#'

nafo_summary_table <- function(comments = rep(NA, 5), status = rep(NA, 5)) {

    if (length(comments) != 5) {
        stop("Comments must be a vector of length = 5 (for each row in table)")
    }
    if (length(status) != 5) {
        stop("Status must be a vector of length = 5 (for each row in table)")
    }
    if (any(status) %in% c("OK", "Intermediate", "Not accomplished", "Unknown")) {
        stop("status must be one of 'OK', 'Intermediate', 'Not accomplished', 'Unknown'")
    }
    stock_summary_df <- data.frame(
        "Principles" = c("Restore or maintain at Bmsy",
                         "Eliminate overfishing",
                         "Apply Percautionary Approach",
                         "Minimise harmful impacts on living marine resources and ecosystems",
                         "Preserve marine biodiversity"),
        "Status" = status,
        "Comment" = comments
    )
    names(stock_summary_df) <- c("Convention General Principles", "Status", "Comment/consideration")

    green_light <- system.file("graphics", "green-light.jpg", package = "NAFOdown")
    yellow_light <- system.file("graphics", "yellow-light.jpg", package = "NAFOdown")
    red_light <- system.file("graphics", "red-light.jpg", package = "NAFOdown")
    white_light <- system.file("graphics", "white-light.jpg", package = "NAFOdown")

    summary_table <- flextable(stock_summary_df) %>%
        theme_nafotabs() %>%
        align(align = "left", part = "all") %>%
        compose(
            j = 1, i = 1, part = "body",
            value = as_paragraph("Restore or maintain at ", as_i("B"), as_i(as_sub("MSY")))
        ) %>%
        compose(
            j = 3, i = 1, part = "body",
            value = as_paragraph(as_i("B"), as_i(as_sub("MSY")))
        ) %>%
        compose(
            part="body",j=2,value=as_paragraph('\U25CF')
        ) %>%
        compose(
            part="body",j=2,i = which(stock_summary_df$Status == "Unknown"),value=as_paragraph('\U2B58')
        )%>%
        fontsize(
            part="body",j=2, size=20
        ) %>%
        color(
            part="body",i = which(stock_summary_df$Status == "OK"),j=2, color = "#008450"
        ) %>%
        color(
            part="body",i = which(stock_summary_df$Status == "Intermediate"),j=2, color = "#EFB700"
        ) %>%
        color(
            part="body",i = which(stock_summary_df$Status == "Not accomplished"),j=2, color = "#B81D13"
        ) %>%
        color(
            part="body",i = which(stock_summary_df$Status == "Unknown"),j=2, color = NA
        ) %>%
        color(
            part="body",i = which(is.null(stock_summary_df$Status)),j=2, color = "white"
        ) %>%
        italic(i = 1, part = "header") %>%
        width(width = c(2.9, 0.65, 2.9))

    summary_table

}
