test_that("can load total unit cost model", {
    mod <- unitcost()
    samples <- mod$samples()
    expect_true(mod$is_fitted())
    expect_true(inherits(samples, "mcmc.list"))
    expect_equal(length(samples), 3)
    n_iter_expected <- 1000000
    n_thin_expected <- 100
    expect_equal(
        dim(samples[[1]]),
        c((n_iter_expected) / n_thin_expected, 13)
    )
    expect_equal(
        mod$priors(),
        capturetb_priors(beta.mean = rep(0, 5), beta.precision = rep(0.01, 5))
    )
    expect_equal(mod$covariates(), capturetb_covariates())
    expect_equal(mod$target(), "USD_unitcost_total")
    expect_true(all(mod$n_eff() > 10000))
})

test_that("can load fixed unitcost model", {
    mod <- unitcost_fixed()
    samples <- mod$samples()
    expect_true(mod$is_fitted())
    expect_true(inherits(samples, "mcmc.list"))
    expect_equal(length(samples), 3)
    n_iter_expected <- 1000000
    n_thin_expected <- 100
    expect_equal(
        dim(samples[[1]]),
        c((n_iter_expected) / n_thin_expected, 13)
    )
    expect_equal(
        mod$priors(),
        capturetb_priors(beta.mean = rep(0, 5), beta.precision = rep(0.01, 5))
    )
    expect_equal(mod$covariates(), capturetb_covariates())
    expect_equal(mod$target(), "USD_unitcost_fixed")
    expect_true(all(mod$n_eff() > 10000))
})

test_that("can load ohd unitcost model", {
    mod <- unitcost_ohd()
    samples <- mod$samples()
    expect_true(mod$is_fitted())
    expect_true(inherits(samples, "mcmc.list"))
    expect_equal(length(samples), 3)
    n_iter_expected <- 1000000
    n_thin_expected <- 100
    expect_equal(
        dim(samples[[1]]),
        c((n_iter_expected) / n_thin_expected, 13)
    )
    expect_equal(
        mod$priors(),
        capturetb_priors(beta.mean = rep(0, 5), beta.precision = rep(0.01, 5))
    )
    expect_equal(mod$covariates(), capturetb_covariates())
    expect_equal(mod$target(), "USD_unitcost_ohd")
    expect_true(all(mod$n_eff() > 10000))
})
