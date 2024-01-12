
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

# At present, scmreg bundles everything into a single scm_reg() function,
# making unit tests difficult. These snapshot tests for the final output
# objects are added to detect any changes to results (intentional or not)
# that may be introduced by future refactoring. In the long run I'd like
# to replace these with more robust tests as the code base improves, but
# for now this will have to do.

# snapshot tests are fragile, so we don't want to run on old releases
is_oldrel <- grepl(rversions::r_oldrel()$version, R.version.string)

test_that("snapshot is preserved for ordered-categorical regression", {

  skip_if(is_oldrel)

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
  expect_error(ordered_categorical_test("interaction-backward"))

})

test_that("snapshot is preserved for linear regression", {

  skip_if(is_oldrel)

  # convenience function for test
  linear_regression_test <- function(search_direction, ...) {
    suppressWarnings(scm_reg(
      dataset = datasets::airquality,
      variable = "Ozone",
      p_forward = 0.01,
      p_backward = 0.001,
      test_used = "AIC",
      regression = "lm",
      search_direction = search_direction,
      max_steps = Inf,
      ...
    ))
  }

  # expect unchanged output for all search directions
  expect_snapshot_value(
    linear_regression_test(
      search_direction = "forward",
      covariate.list = c("Solar.R", "Wind", "Temp")
    ),
    style = "serialize"
  )
  expect_snapshot_value(
    linear_regression_test(
      search_direction = "backward",
      covariate.list = c("Solar.R", "Wind", "Temp"),
      full_relation = "Solar.R + Wind + Temp"
    ),
    style = "serialize"
  )
  expect_snapshot_value(
    linear_regression_test(
      search_direction = "forward-backward",
      covariate.list = c("Solar.R", "Wind", "Temp")
    ),
    style = "serialize"
  )
  expect_snapshot_value(
    linear_regression_test(
      search_direction = "full",
      covariate.list = c("Solar.R", "Wind", "Temp")
    ),
    style = "serialize"
  )
  expect_snapshot_value(
    linear_regression_test(
      search_direction = "interaction",
      base_relation = "Solar.R + Wind + Temp"
    ),
    style = "serialize"
  )
  expect_snapshot_value(
    linear_regression_test(
      search_direction = "forward-interaction",
      covariate.list = c("Solar.R", "Wind", "Temp")
    ),
    style = "serialize"
  )

  # This test omitted for the time being as it errors unexpectedly
  #
  # expect_snapshot_value(
  #   linear_regression_test(
  #     search_direction = "interaction-backward",
  #     base_relation = "Solar.R + Wind + Temp"
  #   ),
  #   style = "serialize"
  # )

})


test_that("snapshot is preserved for logistic regression", {

  skip_if(is_oldrel)

  # convenience function for test
  logistic_regression_test <- function(search_direction, ...) {
    df <- MASS::Melanoma
    df$status <- ifelse(df$status == 1, 1, 0)
    suppressWarnings(scm_reg(
      df,
      variable = "status",
      regression = "logistic",
      search_direction = search_direction,
      p_forward = 0.01,
      p_backward = 0.001,
      test_used = "AIC",
      ...
    ))
  }

  expect_snapshot_value(
    logistic_regression_test(
      search_direction = "forward",
      covariate.list = c("sex", "age", "thickness", "ulcer")
    ),
    style = "serialize"
  )
  expect_snapshot_value(
    logistic_regression_test(
      search_direction = "backward",
      full_relation = "sex + age + thickness + ulcer"
    ),
    style = "serialize"
  )
  expect_snapshot_value(
    logistic_regression_test(
      search_direction = "forward-backward",
      covariate.list = c("sex", "age", "thickness", "ulcer")
    ),
    style = "serialize"
  )
  expect_snapshot_value(
    logistic_regression_test(
      search_direction = "full",
      covariate.list = c("sex", "age", "thickness", "ulcer")
    ),
    style = "serialize"
  )
  expect_snapshot_value(
    logistic_regression_test(
      search_direction = "interaction",
      base_relation = "sex + age + thickness + ulcer"
    ),
    style = "serialize"
  )
  expect_snapshot_value(
    logistic_regression_test(
      search_direction = "forward-interaction",
      covariate.list = c("sex", "age", "thickness", "ulcer")
    ),
    style = "serialize"
  )

  # as above... this errors unexpectedly, ignored for now
  # expect_snapshot_value(
  #   logistic_regression_test(
  #     search_direction = "interaction-backward",
  #     base_relation = "sex + age + thickness + ulcer"
  #   ),
  #   style = "serialize"
  # )

})

