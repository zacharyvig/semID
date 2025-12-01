test_that("rules produce the correct output", {
  rules <- get_rules(rule = "*", model_type = "*")
  partable <- lavaanify("y ~ x", warn = FALSE)
  for (fn in names(rules)) {
    expect_named(
      do.call(rules[[fn]], list(partable)),
      c("rule", "pass", "warn", "cond"),
      label = fn, ignore.order = TRUE
      )
  }
})

test_that("rule titles should be less than 22 characters", {
  rules <- get_rules(rule = "*", model_type = "*")
  partable <- lavaanify("y ~ x", warn = FALSE)
  for (fn in names(rules)) {
    title <- do.call(rules[[fn]], list(partable))$rule
    expect_lt(nchar(!!title), 22)
  }
})
