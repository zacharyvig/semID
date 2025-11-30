test_that("rules produce the correct output", {
  rules <- get_rules(rule = "*", model_type = "*")
  list2env(rules, env = environment())
  partable <- lavaanify("y ~ x", warn = FALSE)
  for (fn in names(rules)) {
    expect_named(
      do.call(fn, list(partable)),
      c("rule", "pass", "warn", "cond"),
      label = fn, ignore.order = TRUE
      )
  }
})

test_that("rule titles should be less than 23 characters", {
  rules <- get_rules(rule = "*", model_type = "*")
  list2env(rules, env = environment())
  partable <- lavaanify("y ~ x", warn = FALSE)
  for (fn in names(rules)) {
    title <- do.call(fn, list(partable))$rule
    expect_lt(nchar(!!title), 23)
  }
})
