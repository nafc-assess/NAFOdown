test_that("SCS title tabs are injected in title custom styles", {
  lines <- c(
    '::: {custom-style="Title"}',
    "",
    "__Species X in Divs. Y__ Advice 2026 for 2027",
    "",
    ":::",
    "Body Advice text stays unchanged"
  )

  updated <- NAFOdown:::.inject_scs_title_tabs(lines)

  expect_identical(
    updated[3],
    "__Species X in Divs. Y__`<w:r><w:tab/></w:r>`{=openxml}Advice 2026 for 2027"
  )
  expect_identical(updated[6], lines[6])
})

test_that("SCS title tabs are not duplicated", {
  lines <- c(
    '::: {custom-style="Title"}',
    "__Species X in Divs. Y__`<w:r><w:tab/></w:r>`{=openxml}Advice 2026 for 2027",
    ":::"
  )

  updated <- NAFOdown:::.inject_scs_title_tabs(lines)

  expect_identical(updated, lines)
})

test_that("SCS title tab injection updates markdown files", {
  input_file <- tempfile(fileext = ".md")
  writeLines(c(
    '::: {custom-style="Title"}',
    "__Species X in Divs. Y__ Advice 2026 for 2027",
    ":::"
  ), input_file)

  changed <- NAFOdown:::.inject_scs_title_tabs_file(input_file)

  expect_true(changed)
  expect_true(grepl("<w:tab/>", readLines(input_file, warn = FALSE)[2],
                    fixed = TRUE))
})
