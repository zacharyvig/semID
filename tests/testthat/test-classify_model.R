source(testthat::test_path("..", "test_models.R"))

make_partable <- function(model) {
  lavaan::lavaanify(
    model$model,
    warn = FALSE,
    auto = TRUE,
    model.type = model$type
  )
}

test_that("classify_model identifies reg, cfa, sem, and mlm parts", {
  expect_identical(
    classify_model(make_partable(test_models$reg_pass)),
    "reg"
  )
  expect_identical(
    classify_model(make_partable(test_models$cfa_three_pass)),
    "cfa"
  )
  expect_identical(
    classify_model(make_partable(test_models$sem_scaling_pass)),
    "sem"
  )
  expect_identical(
    classify_model(lavaan::lavaanify("level: 1\n  y ~ x\nlevel: 2\n y ~ x")),
    "mlm"
  )
})