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

  output <- bookdown::render_book()
  # system(paste("open", output))

  expect_true(file.exists(output))
  expect_match(tolower(tools::file_ext(output)), "docx")
})
