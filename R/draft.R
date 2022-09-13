

#' Create a new NAFO document based on a template
#'
#' Create a draft of an R Markdown NAFO document. This function will also import data from a specified directory if the user chooses.
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
    safe.cat <- function(..., file) {
        if (!file.exists(file)) {
            cat(..., file = file)
        }
    }
    template <- paste0("## %s\n\n",
                       "library(NAFOdown)\n\n")
    headers <- list(data = "Preparation of data for model input",
                    analysis = "Run analysis/model, output model results",
                    figures = "Grab results and produce figures. Remember to use theme_nafo()!",
                    tables = "Grab results and produce table Remember to use theme_nafotab()!",
                    report = "This file will basically just run the index.Rmd file. Remember that index.Rmd controls the generation of the document via the _bookdown.yml file. ")
    for (section in names(headers)) {
        if(!dir.exists(file.path(section))){
            dir.create(path = file.path(section))
            message <- sprintf("Folder %s created. \n",file.path(section))

        }else{
            message <- sprintf("Folder %s already exists. Folder not overwritten. \n",file.path(section))
        }
        safe.cat(sprintf(template, headers[[section]], section),
                 file = paste0(section, ".R"))
        cat(message)
    }
    nafo_fill<-function(){
        res<-list()
        res["import"] <- svDialogs::dlg_message("Would you like to import data from an existing folder?", "yesno")$res
        if( res["import"]=="yes"){
            res["dir"] <- svDialogs::dlg_dir(title = "Please select a directory to import")$res

            if(exists("data")){
                filestocopy <- list.files(path=file.path(res["dir"]),full.names = T)
                invisible(file.copy(from=filestocopy, to="data",copy.mode = T))
            }else{
                dir.create("data")
                file.copy("my_folder", getwd(), recursive=TRUE)
            }
            message <- paste0(sprintf("You have pasted files from %s to \n",res["dir"]), getwd(),"/data")
        }else{
            message <- "You have chosen not to import data from another directory \n"
        }
        cat(message)
    }
    nafo_fill()


}

