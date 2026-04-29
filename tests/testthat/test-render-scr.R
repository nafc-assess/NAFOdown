test_that("default SCR template renders", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("bookdown")

  workdir <- tempfile("nafodown-scr-")
  dir.create(workdir)

  old_wd <- getwd()
  setwd(workdir)
  on.exit(setwd(old_wd), add = TRUE)

  NAFOdown::draft("SCR", create_dir = FALSE, edit = FALSE)
  expect_true(file.exists(file.path(workdir, "index.Rmd")))

  output <- bookdown::render_book()

  expect_true(file.exists(output))
  expect_match(tolower(tools::file_ext(output)), "docx")
})
