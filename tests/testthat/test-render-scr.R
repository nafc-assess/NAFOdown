test_that("default SCR template renders", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("bookdown")
  skip_if_not_installed("systemfonts")

  workdir <- tempfile("nafodown-scr-")
  dir.create(workdir)

  old_wd <- getwd()
  setwd(workdir)
  on.exit(setwd(old_wd), add = TRUE)

  NAFOdown::draft("SCR", create_dir = FALSE, edit = FALSE)

  index_file <- file.path(workdir, "index.Rmd")
  expect_true(file.exists(index_file))

  cambria_path <- systemfonts::match_font("Cambria")$path
  if (identical(cambria_path, "")) {
    index_lines <- readLines(index_file)
    index_lines <- index_lines[
      !grepl('flextable::set_flextable_defaults\(font\.family = "Cambria"\)', index_lines, fixed = FALSE) &
        !grepl("theme_set\(theme_nafo\(\)\)", index_lines, fixed = FALSE)
    ]
    writeLines(index_lines, index_file)
  }

  output <- bookdown::render_book()

  expect_true(file.exists(output))
  expect_match(tolower(tools::file_ext(output)), "docx")
})
