# scmreg

<!-- badges: start -->
[![R-CMD-check](https://github.com/certara/scmreg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/certara/scmreg/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/certara/scmreg/branch/master/graph/badge.svg)](https://app.codecov.io/gh/certara/scmreg?branch=master)
<!-- badges: end -->

Perform Stepwise Covariate Modeling (SCM) in R

### Installation
```
# Development
remotes::install_github("certara/scmreg")
```

### Usage

We will be using the `MASS::housing` dataset to perform a ordered categorical regression
using the `scm_reg` function.

``` r
library(scmreg)
library(MASS)

model <- scm_reg(dataset=housing,
                variable='Sat',
                covariate.list = c('Infl','Type','Cont'),
                p_forward=0.01,
                p_backward=0.001,
                test_used = 'AIC',
                regression='ordered-categorical',
                search_direction='forward-backward',
                weights_ordered='Freq',
                max_steps=Inf)
                
tabscm(model)
```
