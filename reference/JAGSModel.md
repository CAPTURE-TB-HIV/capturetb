# JAGSModel R6 Class

This class allows the user to fit and use a mixed effects model with
intercepts that vary by country, facility and output type as well as
fixed covariate effects. It is used for the comparison of different
model structures as in the
`vignette("03_model-comparisons", package = "capturetb")` vignette, and
is the class underlying the [`unitcost`](unitcost.md),
[`unitcost_fixed`](unitcost_fixed.md) and
[`unitcost_ohd`](unitcost_ohd.md) models.

## Details

R6 class for fitting and predicting costs using a JAGS model.

## See also

[runjags::run.jags](https://rdrr.io/pkg/runjags/man/run.jags.html).

bayestestR describe_posterior

[bayesplot::mcmc_trace](https://mc-stan.org/bayesplot/reference/MCMC-traces.html)

[bayesplot::mcmc_acf](https://mc-stan.org/bayesplot/reference/MCMC-diagnostics.html)

[coda::effectiveSize](https://rdrr.io/pkg/coda/man/effectiveSize.html)

[bayesplot::mcmc_areas](https://mc-stan.org/bayesplot/reference/MCMC-intervals.html)

bayestestR describe_posterior

## Methods

### Public methods

- [`JAGSModel$new()`](#method-JAGSModel-new)

- [`JAGSModel$fit()`](#method-JAGSModel-fit)

- [`JAGSModel$predict()`](#method-JAGSModel-predict)

- [`JAGSModel$baselines()`](#method-JAGSModel-baselines)

- [`JAGSModel$covariate_correlation()`](#method-JAGSModel-covariate_correlation)

- [`JAGSModel$target()`](#method-JAGSModel-target)

- [`JAGSModel$training_data()`](#method-JAGSModel-training_data)

- [`JAGSModel$outputs()`](#method-JAGSModel-outputs)

- [`JAGSModel$samples()`](#method-JAGSModel-samples)

- [`JAGSModel$covariates()`](#method-JAGSModel-covariates)

- [`JAGSModel$countries()`](#method-JAGSModel-countries)

- [`JAGSModel$priors()`](#method-JAGSModel-priors)

- [`JAGSModel$centering_values()`](#method-JAGSModel-centering_values)

- [`JAGSModel$is_fitted()`](#method-JAGSModel-is_fitted)

- [`JAGSModel$mcmc_trace()`](#method-JAGSModel-mcmc_trace)

- [`JAGSModel$mcmc_rhat()`](#method-JAGSModel-mcmc_rhat)

- [`JAGSModel$mcmc_acf()`](#method-JAGSModel-mcmc_acf)

- [`JAGSModel$n_eff()`](#method-JAGSModel-n_eff)

- [`JAGSModel$plot_posteriors()`](#method-JAGSModel-plot_posteriors)

- [`JAGSModel$performance()`](#method-JAGSModel-performance)

- [`JAGSModel$plot_residuals()`](#method-JAGSModel-plot_residuals)

- [`JAGSModel$plot_fit()`](#method-JAGSModel-plot_fit)

- [`JAGSModel$k_fold_cv()`](#method-JAGSModel-k_fold_cv)

- [`JAGSModel$leave_one_country_out()`](#method-JAGSModel-leave_one_country_out)

- [`JAGSModel$fitted_parameters()`](#method-JAGSModel-fitted_parameters)

- [`JAGSModel$mcmc_DIC()`](#method-JAGSModel-mcmc_DIC)

- [`JAGSModel$clone()`](#method-JAGSModel-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new model instance.

#### Usage

    JAGSModel$new(
      dat,
      covariates,
      target,
      priors = NULL,
      country_random_effects = TRUE
    )

#### Arguments

- `dat`:

  Data.frame. Training data

- `covariates`:

  Character vector. Names of covariate columns.

- `target`:

  Character. Name of the target variable.

- `priors`:

  List of class "capturetbpriors". Should be created using
  [`capturetb_priors`](capturetb_priors.md). If NULL, non-informative
  priors will be used.

- `country_random_effects`:

  Logical. Whether to include country random effects. Default TRUE.

------------------------------------------------------------------------

### Method `fit()`

Fit the model using JAGS. Requires JAGS and runjags to be installed.

#### Usage

    JAGSModel$fit(
      n.chains = 3,
      n.iter = 1e+06,
      n.burnin = 5000,
      n.adapt = 5000,
      n.thin = 100,
      seed = NULL,
      ...
    )

#### Arguments

- `n.chains`:

  Integer. Number of MCMC chains. Default is 3.

- `n.iter`:

  Integer. Number of total iterations per chain. Default is 1000000.

- `n.burnin`:

  Integer. Number of burn-in iterations. Default is 5000.

- `n.adapt`:

  Integer. Number of adaptation iterations. Default is 5000.

- `n.thin`:

  Integer. Thinning interval. Default is 100.

- `seed`:

  Optonal integer. Used to seed both the R and JAGS random generators
  for reproducible results.

- `...`:

  Additional arguments passed to
  [runjags::run.jags](https://rdrr.io/pkg/runjags/man/run.jags.html).

#### Returns

Self (invisibly) for method chaining.

------------------------------------------------------------------------

### Method [`predict()`](https://rdrr.io/r/stats/predict.html)

Generate predictions from the fitted model.

#### Usage

    JAGSModel$predict(
      dat,
      scale = "log",
      summarised = FALSE,
      centrality = "mean",
      ci = 0.95,
      ci_type = "predictive",
      test = NULL,
      ...
    )

#### Arguments

- `dat`:

  New input data for predictions. This should be prepared for the model
  using [prepare_covariates](prepare_covariates.md).

- `scale`:

  One of "log" or "natural". Default "log".

- `summarised`:

  Logical. If TRUE, summarises predictions using
  [bayestestR::describe_posterior](https://easystats.github.io/bayestestR/reference/describe_posterior.html).
  See
  [bayestestR::describe_posterior](https://easystats.github.io/bayestestR/reference/describe_posterior.html)
  for full documentation of available arguments. Default FALSE.

- `centrality`:

  The point-estimates (centrality indices) to compute. Default "mean".

- `ci`:

  Value or vector of probability of the credible interval (between 0
  and 1) to be estimated. Default `0.95` (`95%`).

- `ci_type`:

  One of "mean" or "predictive". Specify whether you want the credible
  interval around the mean prediction, or the predictive interval.
  Default "predictive".

- `test`:

  The indices of effect existence to compute. Default NULL. See
  [bayestestR::describe_posterior](https://easystats.github.io/bayestestR/reference/describe_posterior.html)
  for options.

- `...`:

  Other arguments that will be passed to
  [bayestestR::describe_posterior](https://easystats.github.io/bayestestR/reference/describe_posterior.html).

- `ci_method`:

  The type of index used for the Credible Interval. You probably want
  either ETI (Equal Tailed Interval) or HDI (Highest Density Interval).
  Default ETI. See
  [bayestestR::describe_posterior](https://easystats.github.io/bayestestR/reference/describe_posterior.html)
  for all options.

#### Returns

If summarised=FALSE, matrix of predicted costs with rows = simulations,
columns = input rows. If summarised=TRUE, data.frame with central point
estimates and credible intervals.

------------------------------------------------------------------------

### Method `baselines()`

View distribution of binary characteristics across countries

#### Usage

    JAGSModel$baselines()

#### Returns

A data frame with one row per country (`fc_country`), including the
total number of records for that country (`n_total`) and the count of
`TRUE` values for each logical covariate.

------------------------------------------------------------------------

### Method `covariate_correlation()`

View correlations between covariates in the training data

#### Usage

    JAGSModel$covariate_correlation(plot = TRUE)

#### Arguments

- `plot`:

  Logical. If TRUE, return a
  [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
  object. If FALSE return a correlation matrix. Default TRUE.

#### Returns

[ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object or correlation matrix

------------------------------------------------------------------------

### Method `target()`

Get the name of the target variable.

#### Usage

    JAGSModel$target()

#### Returns

character scalar.

------------------------------------------------------------------------

### Method `training_data()`

Get the data used to fit the model.

#### Usage

    JAGSModel$training_data()

#### Returns

data.frame.

------------------------------------------------------------------------

### Method [`outputs()`](outputs.md)

Get the outputs used for random effects.

#### Usage

    JAGSModel$outputs()

#### Returns

factor.

------------------------------------------------------------------------

### Method `samples()`

Get the fitted MCMC samples.

#### Usage

    JAGSModel$samples()

#### Returns

[coda::mcmc.list](https://rdrr.io/pkg/coda/man/mcmc.list.html) object or
NULL if not fitted.

------------------------------------------------------------------------

### Method `covariates()`

Get the covariates used in the model.

#### Usage

    JAGSModel$covariates()

#### Returns

Character vector of covariate names.

------------------------------------------------------------------------

### Method `countries()`

Get the countries from the training data.

#### Usage

    JAGSModel$countries()

#### Returns

Character vector of country names.

------------------------------------------------------------------------

### Method `priors()`

Get the priors used in the model.

#### Usage

    JAGSModel$priors()

#### Returns

List of prior parameters of class 'capturetbpriors'.

------------------------------------------------------------------------

### Method `centering_values()`

Get any centering values used to center covariates in the training data.

#### Usage

    JAGSModel$centering_values()

#### Returns

List of centering values.

------------------------------------------------------------------------

### Method `is_fitted()`

Check if the model has been fitted.

#### Usage

    JAGSModel$is_fitted()

#### Returns

Logical indicating if model is fitted.

------------------------------------------------------------------------

### Method `mcmc_trace()`

Create trace plots for MCMC chains using
[bayesplot::mcmc_trace](https://mc-stan.org/bayesplot/reference/MCMC-traces.html).

#### Usage

    JAGSModel$mcmc_trace(...)

#### Arguments

- `...`:

  Additional arguments passed to
  [bayesplot::mcmc_trace](https://mc-stan.org/bayesplot/reference/MCMC-traces.html).

#### Returns

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object showing trace plots.

------------------------------------------------------------------------

### Method `mcmc_rhat()`

Compute and plot R-hat convergence diagnostics.

#### Usage

    JAGSModel$mcmc_rhat(par = NULL)

#### Arguments

- `par`:

  Optional character vector of parameter names to plot.

#### Returns

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object showing R-hat diagnostics.

------------------------------------------------------------------------

### Method `mcmc_acf()`

Create autocorrelation plots for MCMC chains using
[bayesplot::mcmc_acf](https://mc-stan.org/bayesplot/reference/MCMC-diagnostics.html).

#### Usage

    JAGSModel$mcmc_acf(...)

#### Arguments

- `...`:

  Additional arguments passed to
  [bayesplot::mcmc_acf](https://mc-stan.org/bayesplot/reference/MCMC-diagnostics.html).

#### Returns

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object showing autocorrelation plots.

------------------------------------------------------------------------

### Method `n_eff()`

Computes the effective sample size of the posterior samples using the
[coda::effectiveSize](https://rdrr.io/pkg/coda/man/effectiveSize.html)
function.

#### Usage

    JAGSModel$n_eff()

#### Returns

A named numeric vector.

------------------------------------------------------------------------

### Method `plot_posteriors()`

Plot posterior distributions using
[bayesplot::mcmc_areas](https://mc-stan.org/bayesplot/reference/MCMC-intervals.html).

#### Usage

    JAGSModel$plot_posteriors(prob = 0.9, ...)

#### Arguments

- `prob`:

  Numeric. Density to highlight. Default 0.9.

- `...`:

  Additional arguments passed to
  [bayesplot::mcmc_areas](https://mc-stan.org/bayesplot/reference/MCMC-intervals.html).

#### Returns

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object showing posterior distributions.

------------------------------------------------------------------------

### Method `performance()`

Calculate model performance metrics on known data

This method evaluates the fitted model's performance by comparing
predictions to known costs and computing mean absolute error (MAE), root
mean square error (RMSE), Bayesian R2, credible interval coverage and
median credible interval width. By default the model training data is
used, but a different dataset can also be provided.

#### Usage

    JAGSModel$performance(
      scale = "natural",
      conditional = FALSE,
      by_country = FALSE,
      dat = NULL
    )

#### Arguments

- `scale`:

  One of "log" or "natural". Default "log".

- `conditional`:

  Logical. If TRUE, returns conditional performance. If FALSE, returns
  performance marginalised over facility random effects. Default FALSE.

- `by_country`:

  Logical. If TRUE, returns metrics calculated on country sub-groups.
  Default FALSE.

- `dat`:

  Optional data prepared using
  [`prepare_covariates()`](prepare_covariates.md). If provided, uses
  this data for performance calculation instead of the training data.

#### Returns

A data.frame with performance metrics:

- country: Only present if by_country = TRUE

- mae: Mean Absolute Error between observed and predicted values

- rmse: Root Mean Square Error between observed and predicted values

- bayesian_r2: Mean Bayesian R2 estimate.

- ci_coverage: Proportion of observations within 95% credible intervals

- median_ci: The median width of 95% credible intervals

#### Examples

    model <- unitcost()
    model$performance()

------------------------------------------------------------------------

### Method `plot_residuals()`

Create a residual plot for for diagnosing model fit.

This method generates a diagnostic plot showing residuals (observed
minus predicted values) against fitted values. Residuals are on the log
scale as the model is fitted on a log scale. The plot includes a
reference line at zero, a LOESS smooth curve to identify patterns, and
points colored by country.

#### Usage

    JAGSModel$plot_residuals(add_smooth = TRUE, color_by_country = TRUE)

#### Arguments

- `add_smooth`:

  Logical. Whether to add a LOESS smooth curve to identify patterns in
  residuals. Default TRUE.

- `color_by_country`:

  Logical. Whether to color points by country. Default TRUE.

#### Returns

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object showing residuals vs fitted values.

------------------------------------------------------------------------

### Method `plot_fit()`

Create a scatter plot of observed vs predicted values to assess fit.

This method generates a diagnostic plot showing the relationship between
observed and predicted values on the training data, with a reference
line for perfect predictions and optional confidence intervals.

#### Usage

    JAGSModel$plot_fit(
      scale = "log",
      conditional = FALSE,
      include_ci = TRUE,
      color_by_country = TRUE
    )

#### Arguments

- `scale`:

  One of "log" or "natural". Default "log".

- `conditional`:

  Logical. If TRUE, shows full conditional fit. If FALSE, shows marginal
  fit. Default FALSE.

- `include_ci`:

  Logical. Whether to show predictive intervals as error bars. Default
  TRUE.

- `color_by_country`:

  Logical. Whether to color points by country. Default TRUE.

#### Returns

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object showing observed vs predicted values.

#### Examples

    \dontrun{
    model <- JAGSModel$new()
    model$fit()
    p <- model$plot_fit()
    print(p)

    # Natural scale without confidence intervals
    p2 <- model$plot_fit(scale = "natural", include_ci = FALSE)
    print(p2)
    }

------------------------------------------------------------------------

### Method `k_fold_cv()`

Perform k-fold cross-validation.

#### Usage

    JAGSModel$k_fold_cv(k_folds = 5, scale = "log", seed = NULL, ...)

#### Arguments

- `k_folds`:

  Integer. Number of folds for cross-validation. Default is 5.

- `scale`:

  One of "log" or "natural". Default "log".

- `seed`:

  Integer. Optional random seed for reproducible fold assignment and
  model runs. Default NULL.

- `...`:

  Additional arguments passed to the fit() method.

#### Returns

Data.frame with predictions from cross-validation, including fold
assignments and observed values.

------------------------------------------------------------------------

### Method `leave_one_country_out()`

Perform leave-one-country-out cross-validation.

#### Usage

    JAGSModel$leave_one_country_out(scale = "log", seed = NULL, ...)

#### Arguments

- `scale`:

  One of "log" or "natural". Default "log".

- `seed`:

  Integer. Optional random seed for reproducible fold assignment and
  model runs. Default NULL.

- `...`:

  Additional arguments passed to the fit() method.

#### Returns

Data.frame with predictions from cross-validation, including country and
observed values.

------------------------------------------------------------------------

### Method `fitted_parameters()`

Extract fitted model parameters with credible intervals.

This method summarises fitted parameters using
[bayestestR::describe_posterior](https://easystats.github.io/bayestestR/reference/describe_posterior.html).
See
[bayestestR::describe_posterior](https://easystats.github.io/bayestestR/reference/describe_posterior.html)
for full documentation of available argument.

#### Usage

    JAGSModel$fitted_parameters(
      centrality = "mean",
      ci = 0.95,
      ci_method = "eti",
      test = NULL,
      ...
    )

#### Arguments

- `centrality`:

  The point-estimates (centrality indices) to compute. Default "mean".

- `ci`:

  Value or vector of probability of the CI (between 0 and 1) to be
  estimated. Default `0.95` (`95%`).

- `ci_method`:

  The type of index used for Credible Interval. Default ETI.

- `test`:

  The indices of effect existence to compute. Default NULL.

- `...`:

  Other arguments that will be passed to
  [bayestestR::describe_posterior](https://easystats.github.io/bayestestR/reference/describe_posterior.html).

#### Returns

A data.frame of parameter summaries

#### Examples

    model <- unitcost()
    params <- model$fitted_parameters()
    print(params)

    # 90% credible intervals
    params_90 <- model$fitted_parameters(ci = 0.9)

------------------------------------------------------------------------

### Method `mcmc_DIC()`

Retrieve penalized deviance statistics.

This method returns cached penalized deviance statistics created at the
time of model fitting using.

#### Usage

    JAGSModel$mcmc_DIC(summarised = TRUE)

#### Arguments

- `summarised`:

  Logical. If TRUE (default) return the total DIC as a single numeric
  value. If FALSE, return all DIC samples.

#### Returns

If summarised = TRUE, a numeric scalar of the total DIC. If summarised =
FALSE, an object of class "dic"; see
[`rjags::dic.samples()`](https://rdrr.io/pkg/rjags/man/dic.samples.html).

#### Examples

    mod <- unitcost()
    mod$mcmc_DIC()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    JAGSModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r

## ------------------------------------------------
## Method `JAGSModel$performance`
## ------------------------------------------------

model <- unitcost()
#> Multiple outputs detected. Including output-level random effects in model.
model$performance()
#>        mae     rmse ci_coverage median_ci bayesian_r2
#> 1 5.447124 8.007143   0.9690909  26.73596   0.4283205

## ------------------------------------------------
## Method `JAGSModel$plot_fit`
## ------------------------------------------------

if (FALSE) { # \dontrun{
model <- JAGSModel$new()
model$fit()
p <- model$plot_fit()
print(p)

# Natural scale without confidence intervals
p2 <- model$plot_fit(scale = "natural", include_ci = FALSE)
print(p2)
} # }


## ------------------------------------------------
## Method `JAGSModel$fitted_parameters`
## ------------------------------------------------

model <- unitcost()
#> Multiple outputs detected. Including output-level random effects in model.
params <- model$fitted_parameters()
print(params)
#> Summary of Posterior Distribution
#> 
#> Parameter         |  Mean |         95% CI
#> ------------------------------------------
#> alpha             |  1.87 | [ 1.42,  2.32]
#> beta[1]           | -0.39 | [-0.62, -0.16]
#> beta[2]           |  0.26 | [ 0.02,  0.50]
#> beta[3]           |  0.29 | [-0.06,  0.63]
#> beta[4]           |  0.30 | [-0.06,  0.66]
#> beta[5]           |  0.55 | [ 0.15,  0.97]
#> beta[6]           |  0.38 | [-0.13,  0.88]
#> beta[7]           | -0.20 | [-0.29, -0.12]
#> sigma             |  0.35 | [ 0.33,  0.38]
#> sigma_c           |  0.18 | [ 0.04,  0.38]
#> country_effect[1] |  0.15 | [-0.08,  0.44]
#> country_effect[2] |  0.13 | [-0.10,  0.41]
#> country_effect[3] | -0.18 | [-0.49,  0.05]
#> country_effect[4] |  0.04 | [-0.20,  0.29]
#> country_effect[5] | -0.13 | [-0.41,  0.10]
#> sigma_f           |  0.49 | [ 0.42,  0.58]
#> sigma_v           |  0.16 | [ 0.09,  0.28]
#> output_effect[1]  | -0.06 | [-0.30,  0.16]
#> output_effect[2]  | -0.20 | [-0.35, -0.07]
#> output_effect[3]  |  0.20 | [ 0.08,  0.32]
#> output_effect[4]  |  0.02 | [-0.26,  0.30]
#> output_effect[5]  |  0.13 | [-0.01,  0.28]
#> output_effect[6]  |  0.04 | [-0.07,  0.16]
#> output_effect[7]  |  0.07 | [-0.18,  0.34]
#> output_effect[8]  | -0.02 | [-0.16,  0.12]
#> output_effect[9]  | -0.02 | [-0.15,  0.12]
#> output_effect[10] |  0.08 | [-0.06,  0.23]
#> output_effect[11] |  0.12 | [-0.11,  0.37]
#> output_effect[12] | -0.09 | [-0.22,  0.03]
#> output_effect[13] | -0.16 | [-0.45,  0.07]
#> output_effect[14] | -0.10 | [-0.30,  0.09]

# 90% credible intervals
params_90 <- model$fitted_parameters(ci = 0.9)

## ------------------------------------------------
## Method `JAGSModel$mcmc_DIC`
## ------------------------------------------------

mod <- unitcost()
#> Multiple outputs detected. Including output-level random effects in model.
mod$mcmc_DIC()
#> [1] 532.199
```
