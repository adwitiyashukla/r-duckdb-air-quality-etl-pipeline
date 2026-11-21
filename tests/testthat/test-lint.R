test_that("the code passes lintr", {
  lints <- lintr::lint_dir(project_root())
  expect_length(lints, 0)
})
