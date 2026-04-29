test_that("default SCR template renders", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("bookdown")

  skeleton <- system.file(
    "rmarkdown/templates/SCR/skeleton/skeleton.Rmd",
    package = "NAFOdown"
  )
  expect_true(file.exists(skeleton))

  workdir <- tempfile("nafodown-scr-")
  dir.create(workdir)

  source_dir <- dirname(skeleton)
  file.copy(source_dir, workdir, recursive = TRUE)
  render_dir <- file.path(workdir, basename(source_dir))

  output <- rmarkdown::render(
    input = file.path(render_dir, "skeleton.Rmd"),
    output_format = "NAFOdown::word_scr",
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )

  expect_true(file.exists(output))
  expect_match(tolower(tools::file_ext(output)), "docx")
})
