test_that("rules produce the correct output", {
  rules <- get_rules(rule = "*", model_type = "*")
  partable <- lavaan::lavaanify("y ~ x", warn = FALSE)
  for (fn in names(rules)) {
    expect_named(
      do.call(rules[[fn]], list(partable)),
      c("rule", "pass", "msgs", "cond"),
      label = fn, ignore.order = FALSE
      )
  }
})

test_that("rule titles should be correct length", {
  rules <- get_rule_names(model_type = "all")
  partable <- lavaan::lavaanify("y ~ x", warn = FALSE)
  test <- capture.output(id(partable))
  header <- grep("Pass", test)
  blank <- sub("^(\\s+)([A-Za-z\\s]+)$", "\\1", test[header], perl = TRUE)
  for (rule in rules) {
    title <- do.call(rule, list(partable))$rule
    expect_lt(nchar(!!title), nchar(blank))
  }
})
