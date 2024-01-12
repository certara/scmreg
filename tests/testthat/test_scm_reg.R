
# check S3 class ----------------------------------------------------------

test_that("class inheritance works", {

  model <- suppressWarnings(
    scm_reg(
      dataset = MASS::housing,
      variable = "Sat",
      covariate.list = c("Infl", "Type", "Cont"),
      p_forward = 0.01,
      p_backward = 0.001,
      test_used = "AIC",
      regression = "ordered-categorical",
      search_direction = "forward-backward",
      weights_ordered = "Freq",
      max_steps = Inf
    )
  )
  table <- tabscm(model)

  expect_true(inherits(model, "scmobject"))
  expect_true(inherits(table, "flextable"))
})


# snapshot tests to detect if updates alter output objects ----------------

test_that("snapshot is preserved for ordered-categorical regression", {

  # convenience function for test
  ordered_categorical_test <- function(search_direction, ...) {
    suppressWarnings(scm_reg(
      dataset = MASS::housing,
      variable = "Sat",
      covariate.list = c("Infl", "Type", "Cont"),
      p_forward = 0.01,
      p_backward = 0.001,
      test_used = "AIC",
      regression = "ordered-categorical",
      search_direction = search_direction,
      weights_ordered = "Freq",
      max_steps = Inf,
      ...
    ))
  }

  # expect unchanged results for supported search directions
  expect_snapshot_value(
    ordered_categorical_test("forward"),
    style = "serialize"
  )
  expect_snapshot_value(
    ordered_categorical_test("backward", full_relation = "Infl + Type + Cont"),
    style = "serialize"
  )
  expect_snapshot_value(
    ordered_categorical_test("forward-backward"),
    style = "serialize"
  )

  # expect errors when unsupported search directions are invoked
  expect_error(ordered_categorical_test("full"))
  expect_error(ordered_categorical_test("interaction"))
  expect_error(ordered_categorical_test("forward-interaction"))
  expect_error(ordered_categorical_test("backward-interaction"))

})



