test_that("can plot posteriors once fitted", {
    model <- MixedEffects$new(dat_cleaned,
        priors = capturetb_priors(
            beta.mean = rep(0, 5),
            beta.precision = rep(0.01, 5)
        )
    )
    expect_error(
        model$plot_posteriors(),
        "Model must be fitted"
    )

    res <- unitcost()$plot_posteriors()
    expect_true(inherits(res, "ggplot"))
})

test_that("posterior snapshot test", {
    skip_on_ci()
    set.seed(1)
    vdiffr::expect_doppelganger("posteriors", unitcost()$plot_posteriors())
})

test_that("can plot trace once fitted", {
    model <- MixedEffects$new(dat_cleaned,
        priors = capturetb_priors(
            beta.mean = rep(0, 5),
            beta.precision = rep(0.01, 5)
        )
    )

    expect_error(
        model$mcmc_trace(),
        "Model must be fitted"
    )

    model <- unitcost()
    res <- model$mcmc_trace()
    expect_true(inherits(res, "ggplot"))
})

test_that("trace snapshot test", {
    skip_on_ci()
    set.seed(1)
    vdiffr::expect_doppelganger("trace", unitcost()$mcmc_trace())
})

test_that("can plot rhat once fitted", {
    model <- MixedEffects$new(dat_cleaned,
        priors = capturetb_priors(
            beta.mean = rep(0, 5),
            beta.precision = rep(0.01, 5)
        )
    )

    expect_error(
        model$mcmc_rhat(),
        "Model must be fitted"
    )

    model <- unitcost()
    res <- model$mcmc_rhat()
    expect_true(inherits(res, "ggplot"))
})

test_that("rhat snapshot test", {
    skip_on_ci()
    set.seed(1)
    vdiffr::expect_doppelganger("rhat", unitcost()$mcmc_rhat())
})

test_that("can plot acf once fitted", {
    model <- MixedEffects$new(dat_cleaned,
        priors = capturetb_priors(
            beta.mean = rep(0, 5),
            beta.precision = rep(0.01, 5)
        )
    )

    expect_error(
        model$mcmc_acf(),
        "Model must be fitted"
    )

    model <- unitcost()
    res <- model$mcmc_acf()
    expect_true(inherits(res, "ggplot"))
})

test_that("acf snapshot test", {
    skip_on_ci()
    set.seed(1)
    vdiffr::expect_doppelganger("acf", unitcost()$mcmc_acf())
})

test_that("can plot fit once fitted", {
    model <- MixedEffects$new(dat_cleaned,
        priors = capturetb_priors(
            beta.mean = rep(0, 5),
            beta.precision = rep(0.01, 5)
        )
    )

    expect_error(
        model$plot_fit(),
        "Model must be fitted"
    )

    model <- unitcost()
    res <- model$plot_fit()
    expect_true(inherits(res, "ggplot"))

    res <- model$plot_fit(include_ci = TRUE)
    expect_true(inherits(res, "ggplot"))
})

test_that("fit snapshot test", {
    skip_on_ci()
    set.seed(1)
    vdiffr::expect_doppelganger(
        "fit with CI",
        model$plot_fit(include_ci = TRUE)
    )
    vdiffr::expect_doppelganger(
        "fit without CI",
        model$plot_fit(include_ci = FALSE)
    )
})

test_that("can plot residuals once fitted", {
    skip_on_ci()
    set.seed(1)
    model <- MixedEffects$new(dat_cleaned,
        priors = capturetb_priors(
            beta.mean = rep(0, 5),
            beta.precision = rep(0.01, 5)
        )
    )

    expect_error(
        model$plot_residuals(),
        "Model must be fitted"
    )

    model <- unitcost()
    res <- model$plot_residuals()
    expect_true(inherits(res, "ggplot"))
})

test_that("residuals snapshot test", {
    skip_on_ci()
    set.seed(1)
    vdiffr::expect_doppelganger(
        "residuals",
        model$plot_residuals()
    )
})
