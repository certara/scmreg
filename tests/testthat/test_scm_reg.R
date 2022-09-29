test_that("class inheritance works", {

  model <- suppressWarnings(
    scm_reg(
      dataset = MASS::housing,
      variable = 'Sat',
      covariate.list = c('Infl', 'Type', 'Cont'),
      p_forward = 0.01,
      p_backward = 0.001,
      test_used = 'AIC',
      regression = 'ordered-categorical',
      search_direction = 'forward-backward',
      weights_ordered = 'Freq',
      max_steps = Inf
    )
  )

  table <- tabscm(model)

  expect_true(inherits(model, "scmobject"))

  expect_true(inherits(table, "flextable"))

})
