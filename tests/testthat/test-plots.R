test_that("can plot posteriors once fitted", {
    model <- MixedEffects$new(dat_cleaned)

    expect_error(
        model$plot_posteriors(),
        "Model must be fitted"
    )

    model$fit(n.iter = 1000, n.chain = 2, n.thin = 1, n.burnin = 100)

    res <- model$plot_posteriors()
    expect_true(inherits(res, "ggplot"))
})

test_that("can plot trace once fitted", {
    model <- MixedEffects$new(dat_cleaned)

    expect_error(
        model$mcmc_trace(),
        "Model must be fitted"
    )

    model <- unitcost()
    res <- model$mcmc_trace()
    expect_true(inherits(res, "ggplot"))
})

test_that("can plot rhat once fitted", {
    model <- MixedEffects$new(dat_cleaned)

    expect_error(
        model$mcmc_rhat(),
        "Model must be fitted"
    )

    model <- unitcost()
    res <- model$mcmc_rhat()
    expect_true(inherits(res, "ggplot"))
})

test_that("can plot acf once fitted", {
    model <- MixedEffects$new(dat_cleaned)

    expect_error(
        model$mcmc_acf(),
        "Model must be fitted"
    )

    model <- unitcost()
    res <- model$mcmc_acf()
    expect_true(inherits(res, "ggplot"))
})
