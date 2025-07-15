test_that("FixedEffects model can be initialised, fitted, and used for predictions", {
    model <- FixedEffects$new(
        dat = dat_cleaned, covariates = c("logVisits", "logVisitsPP_TB"),
        priors = capturetb_priors(beta.mean = c(0, 0), beta.precision = c(1, 1))
    )
    fit <- suppressWarnings(model$fit(n.iter = 500))
    expect_true(model$is_fitted())
    samples <- model$samples()
    expect_true(inherits(samples, "mcmc.list"))
    preds <- model$predict(dat_cleaned[1, ])

    expect_true(is.matrix(preds))
    expect_equal(ncol(preds), 1)
    expect_true(nrow(preds) > 0)
})

test_that("FixedEffects model works for a single covariate", {
    model <- FixedEffects$new(
        dat = dat_cleaned, covariates = c("logVisits"),
        priors = capturetb_priors()
    )
    fit <- suppressWarnings(model$fit(n.iter = 500))
    expect_true(model$is_fitted())
    samples <- model$samples()
    expect_true(inherits(samples, "mcmc.list"))
    preds <- model$predict(dat_cleaned[1, ])

    expect_true(is.matrix(preds))
    expect_equal(ncol(preds), 1)
    expect_true(nrow(preds) > 0)
})
