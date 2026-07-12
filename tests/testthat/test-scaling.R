source(testthat::test_path("..", "test_models.R"))

make_partable <- function(model) {
  lavaan::lavaanify(
    model$model,
    warn = FALSE,
    auto = TRUE,
    model.type = model$type
  )
}

test_that("scaling returns logical and object outputs", {
  logical_out <- scaling(
    make_partable(test_models$sem_scaling_pass),
    lv = "L1",
    return.type = "logical"
  )
  object_out <- scaling(
    make_partable(test_models$sem_scaling_pass),
    lv = "L1",
    return.type = "object"
  )

  expect_identical(logical_out, c(L1 = TRUE))
  expect_s3_class(object_out, "semscale")
  expect_true(object_out$Scaling[[1]]$scaled)
  expect_identical(object_out$Scaling[[1]]$lv, "L1")
})

test_that("scaling does not treat a two-indicator factor with one fixed loading as scaled", {
  model <- paste(
    "f1 =~ x1 + x2",
    "x1 ~~ x1",
    "x2 ~~ x2",
    sep = "\n"
  )

  out <- scaling(lavaan::lavaanify(model, warn = FALSE, auto = TRUE, model.type = "sem"),
                 lv = "f1",
                 return.type = "logical")

  expect_identical(out, c(f1 = FALSE))
})

test_that("scaling printing shows success and failure details", {
  pass_out <- capture.output(
    print(
      scaling(make_partable(test_models$sem_scaling_pass), lv = "L1"),
      include.msgs = TRUE
    )
  )
  fail_out <- capture.output(
    print(
      scaling(make_partable(test_models$sem_two_emitted_paths_fail), lv = "L1"),
      include.msgs = TRUE
    )
  )

  expect_true(any(grepl("Latent Variable Scaling", pass_out)))
  expect_true(any(grepl("Scaling method", pass_out)))
  expect_true(any(grepl("L1", pass_out)))

  expect_true(any(grepl("Latent Variable Scaling", fail_out)))
  expect_true(any(grepl("Scaling error", fail_out)))
  expect_true(any(grepl("One indicator", fail_out)))
})