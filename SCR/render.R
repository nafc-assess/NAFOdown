
## TODO: fix the level of the headings for the SCR
rmarkdown::render("README.Rmd",
                  output_format = "NAFOdown::word_scr",
                  output_file = "SCR/NAFOdown_SCR.docx")
