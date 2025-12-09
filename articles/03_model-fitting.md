# Model Fitting

``` r
library(capturetb)
library(ggplot2)
library(dplyr)
```

This vignette demonstrates how the `unitcost`, `unitcost_fixed` and
`unitcost_ohd` models were fitted and sensitivity analyses performed. It
also shows how models could be fit using different covariates, target
variable, training data or priors.

The following functions were included in the package to make the process
of model development transparent and reproducible, but if you want to
use the final `capturetb` models to predict costs you should read
[`vignette("01_unitcost-model-predictions")`](../articles/01_unitcost-model-predictions.md)
and
[`vignette("02_combining-predictions-data")`](../articles/02_combining-predictions-data.md)
instead.

## Creating a model instance

A model instance requires a list of covariates, a target variable to
predict, training data, and priors for parameters.

``` r
covariates <- c("logVisits", "logVisitsPP", "logVisitsPP_TB", "urban", "public")

target <- "ID_unitcost_total"

# Specifying priors for the fixed effects
# One beta coefficient for each covariate
# Other parameters will take default values
priors <- capturetb::capturetb_priors(
  beta.mean = rep(0, length(covariates)),
  beta.precision = rep(0.01, length(covariates))
)

data <- get_data(output_name = "op_diagnosticvisit") 
# or provide your own data;
# see capturetb::outputs() for all output data 
# installed with the package
```

We now create an instance of the
[`capturetb::JAGSModel`](../reference/JAGSModel.md) class to fit a model
with fixed covariate effects and country level random effects. If the
data provided has more than one unique output type in the `output`
column, the model will include facility and visit type effects. If the
data has only one unique output type, no facility or visit type effects
will be included.

``` r
model <- capturetb::JAGSModel$new(
  dat = data,
  covariates = covariates,
  target = target,
  priors = priors
)
#> Warning in initialize(...): Removed 3 rows with missing data.
#> Single output type detected. Not including output-level random effects in model.
```

Priors can be visualised by calling
`plot(priors, par = "name_of_param")`:

``` r
library(gridExtra)
#> 
#> Attaching package: 'gridExtra'
#> The following object is masked from 'package:dplyr':
#> 
#>     combine
priors <- model$priors()
plots <- list()
for(i in 1:3) {
  plots[[i]] <- plot(priors, par = paste0("beta[", i, "]"))
}
plots[[4]] <- plot(priors, par = "sigma_c")
plots[[5]] <- plot(priors, par = "sigma")
do.call(grid.arrange, c(plots, ncol = 3))
#> Warning: Removed 3341 rows containing missing values or values outside the scale range
#> (`geom_line()`).
#> Removed 3341 rows containing missing values or values outside the scale range
#> (`geom_line()`).
```

![](03_model-fitting_files/figure-html/beta-priors-1.png)

## Fitting the model

Under the hood the JAGSModel class uses JAGS (Just Another Gibbs
Sampler) to fit a multilevel linear regression model. To fit the model,
[JAGS](https://sourceforge.net/projects/mcmc-jags/) and the
[runjags](https://cran.r-project.org/web/packages/runjags/index.html)
package must be installed on your machine.

For the purpose of the vignette, we’ll use fewer iterations than
recommended for faster computation:

``` r
# Fit the model with reduced iterations for demonstration
# In practice, you might want to use the defaults (n.iter = 100000)
model$fit(
 n.iter = 20000,
 n.burnin = 1000,
 n.thin = 5,
 n.chains = 2
)
```

``` r
# Check summary statistics of the fitted samples
fitted_samples <- model$samples()
summary(fitted_samples)
#> 
#> Iterations = 6001:25996
#> Thinning interval = 5 
#> Number of chains = 2 
#> Sample size per chain = 4000 
#> 
#> 1. Empirical mean and standard deviation for each variable,
#>    plus standard error of the mean:
#> 
#>                       Mean      SD  Naive SE Time-series SE
#> alpha              3.62002 0.55972 0.0062578      0.0387141
#> beta[1]           -0.07916 0.05181 0.0005792      0.0033275
#> beta[2]            0.04268 0.05528 0.0006180      0.0007040
#> beta[3]           -0.35453 0.09824 0.0010984      0.0024006
#> beta[4]            0.39430 0.13562 0.0015163      0.0021948
#> beta[5]           -0.31318 0.12837 0.0014352      0.0017950
#> sigma              0.56512 0.04165 0.0004656      0.0004773
#> sigma_c            0.56903 0.39477 0.0044136      0.0121861
#> country_effect[1] -0.03716 0.30555 0.0034161      0.0121271
#> country_effect[2]  0.02618 0.31565 0.0035290      0.0123806
#> country_effect[3] -0.46875 0.31414 0.0035122      0.0114606
#> country_effect[4]  0.46854 0.31055 0.0034721      0.0120761
#> country_effect[5]  0.08089 0.31692 0.0035432      0.0131170
#> 
#> 2. Quantiles for each variable:
#> 
#>                       2.5%       25%      50%      75%    97.5%
#> alpha              2.47839  3.247150  3.63254  4.00015  4.66134
#> beta[1]           -0.17957 -0.114588 -0.07946 -0.04372  0.01984
#> beta[2]           -0.06517  0.004994  0.04179  0.07951  0.15225
#> beta[3]           -0.54828 -0.420572 -0.35305 -0.28850 -0.16416
#> beta[4]            0.12865  0.302820  0.39516  0.48607  0.65933
#> beta[5]           -0.56077 -0.400754 -0.31262 -0.22548 -0.06667
#> sigma              0.49027  0.536212  0.56257  0.59111  0.65422
#> sigma_c            0.19771  0.339117  0.46509  0.66451  1.60462
#> country_effect[1] -0.65702 -0.202264 -0.03776  0.12849  0.60163
#> country_effect[2] -0.61630 -0.141864  0.02641  0.19213  0.68423
#> country_effect[3] -1.14263 -0.636462 -0.45856 -0.29007  0.14677
#> country_effect[4] -0.11526  0.292721  0.44983  0.62761  1.16530
#> country_effect[5] -0.54178 -0.098730  0.06770  0.25472  0.78436
```

### Diagnostics

Several functions are available on the model class to check whether the
model has converged:

Rhat:

``` r
model$mcmc_rhat(par = paste0("beta[", 1:length(covariates), "]"))
```

![](03_model-fitting_files/figure-html/rhat-1.png)

Trace:

``` r
model$mcmc_trace(regex_pars = "beta")
```

![](03_model-fitting_files/figure-html/trace-1.png)

Auto-correlation:

``` r
model$mcmc_acf(regex_pars = "beta")
```

![](03_model-fitting_files/figure-html/acf-1.png)

Effective sample size:

``` r
knitr::kable(model$n_eff())
```

|                     |         x |
|:--------------------|----------:|
| alpha               |  210.2480 |
| beta\[1\]           |  247.1581 |
| beta\[2\]           | 6246.7406 |
| beta\[3\]           | 1688.4610 |
| beta\[4\]           | 3818.3265 |
| beta\[5\]           | 5121.7265 |
| sigma               | 7627.9881 |
| sigma_c             | 1092.3314 |
| country_effect\[1\] |  634.8278 |
| country_effect\[2\] |  655.6080 |
| country_effect\[3\] |  751.8475 |
| country_effect\[4\] |  660.1804 |
| country_effect\[5\] |  587.7512 |

We can also plot the posterior distributions of each parameter:

``` r
model$plot_posteriors(pars = paste0("beta[", 1:length(covariates), "]")) + 
  ggplot2::scale_y_discrete(labels = covariates)
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
```

![](03_model-fitting_files/figure-html/post-1.png)

``` r
model$plot_posteriors(pars = paste0("country_effect[", 1:5, "]"))
```

![](03_model-fitting_files/figure-html/country-1.png)

## Generating predictions

We can now generate predictions for the training data and evaluate model
fit:

``` r
# Generate predictions for the training data on log scale
dat <- model$training_data()
predictions <- model$predict(dat, scale = "log", summarised = TRUE)
head(predictions)
#> Summary of Posterior Distribution
#> 
#> Observation | Mean |       95% CI
#> ---------------------------------
#> 1           | 3.46 | [2.26, 4.69]
#> 2           | 2.66 | [1.51, 3.81]
#> 3           | 2.09 | [0.93, 3.24]
#> 4           | 1.85 | [0.71, 2.96]
#> 5           | 2.26 | [1.11, 3.41]
#> 6           | 2.28 | [1.14, 3.42]

# Various measures of fit
performance <- model$performance(scale = "log")
knitr::kable(performance)
```

|       mae |     rmse | ci_coverage | median_ci | bayesian_r2 |
|----------:|---------:|------------:|----------:|------------:|
| 0.4342712 | 0.537294 |   0.9716981 |  2.327685 |   0.4971605 |

## Visualising results

### 1. Predicted vs Observed

``` r
# Create scatter plot of predicted vs observed values
model$plot_fit(include_ci = FALSE, scale = "log")
```

![](03_model-fitting_files/figure-html/scatter-plot-1.png)

### 2. 95% Credible Prediction Intervals

``` r
# Plot with prediction intervals
model$plot_fit(include_ci = TRUE, scale = "log")
```

![](03_model-fitting_files/figure-html/prediction-intervals-1.png)

### 3. Residuals

``` r
model$plot_residuals(add_smooth = TRUE, color_by_country = TRUE)
#> Warning in private$.predict(dat, conditional = TRUE): conditional = TRUE has no
#> effect when there is only one output type
#> `geom_smooth()` using formula = 'y ~ x'
```

![](03_model-fitting_files/figure-html/residuals-1.png)

### 4. Country-Specific perfomance

``` r
# Performance by country
country_performance <- model$performance(by_country = TRUE)
colnames(country_performance) <- c("Country", "MAE",
 "RMSE", "95% CI Coverage", "Median CI Width", "Bayesian R-squ")

knitr::kable(country_performance)
```

| Country     |      MAE |     RMSE | 95% CI Coverage | Median CI Width | Bayesian R-squ |
|:------------|---------:|---------:|----------------:|----------------:|---------------:|
| Ethiopia    | 6.910456 | 9.416620 |       0.9200000 |        32.95744 |      0.3910709 |
| Georgia     | 6.806352 | 7.992778 |       1.0000000 |        52.90886 |      0.4400111 |
| India       | 3.903570 | 4.998774 |       1.0000000 |        20.35975 |      0.3529980 |
| Kenya       | 6.985575 | 9.355108 |       1.0000000 |        38.86220 |      0.4291894 |
| Philippines | 4.520270 | 5.514123 |       0.9583333 |        22.73329 |      0.4836470 |

``` r
model$plot_fit() + 
  ggplot2::facet_wrap(~country, scales = "free")
```

![](03_model-fitting_files/figure-html/country-plot-1.png)

### 5. Out-of-sample performance

We can check for overfitting and estimate out-of-sample performance
using k-fold cross-validation. Here we use 3 folds for quick
compilation; in practice, 10 or 20 folds would give a more accurate
picture.

``` r
res <- model$k_fold_cv(k_folds = 3, 
  n.iter = 10000,
  n.burnin = 1000,
  n.adapt = 1000, 
  scale = "log")
#> Processing fold 1 of 3
#> Single output type detected. Not including output-level random effects in model.
#> Calling 3 simulations using the parallel method...
#> Following the progress of chain 1 (the program will wait for all chains
#> to finish before continuing):
#> Welcome to JAGS 4.3.2 on Tue Dec  9 15:40:54 2025
#> JAGS is free software and comes with ABSOLUTELY NO WARRANTY
#> Loading module: basemod: ok
#> Loading module: bugs: ok
#> . . Reading data file data.txt
#> . Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 70
#>    Unobserved stochastic nodes: 83
#>    Total graph size: 879
#> . Reading parameter file inits1.txt
#> . Initializing model
#> . Adapting 1000
#> -------------------------------------------------| 1000
#> ++++++++++++++++++++++++++++++++++++++++++++++++++ 100%
#> Adaptation successful
#> . Updating 1000
#> -------------------------------------------------| 1000
#> ************************************************** 100%
#> . . . . . . Updating 10000
#> -------------------------------------------------| 10000
#> ************************************************** 100%
#> . . . . Updating 0
#> . Deleting model
#> . 
#> All chains have finished
#> Simulation complete.  Reading coda files...
#> Coda files loaded successfully
#> Finished running the simulation
#> Compiling rjags model and adapting for 1000 iterations...
#> Obtaining DIC samples from 100 iterations...
#> Warning in doTryCatch(return(expr), name, parentenv, handler): Model may not
#> have converged. Max rhat is 1.16745259359348
#> Model fitted successfully with 3 chains and 10000 iterations.
#> Processing fold 2 of 3
#> Single output type detected. Not including output-level random effects in model.
#> Calling 3 simulations using the parallel method...
#> Following the progress of chain 1 (the program will wait for all chains
#> to finish before continuing):
#> Welcome to JAGS 4.3.2 on Tue Dec  9 15:40:55 2025
#> JAGS is free software and comes with ABSOLUTELY NO WARRANTY
#> Loading module: basemod: ok
#> Loading module: bugs: ok
#> . . Reading data file data.txt
#> . Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 71
#>    Unobserved stochastic nodes: 84
#>    Total graph size: 891
#> . Reading parameter file inits1.txt
#> . Initializing model
#> . Adapting 1000
#> -------------------------------------------------| 1000
#> ++++++++++++++++++++++++++++++++++++++++++++++++++ 100%
#> Adaptation successful
#> . Updating 1000
#> -------------------------------------------------| 1000
#> ************************************************** 100%
#> . . . . . . Updating 10000
#> -------------------------------------------------| 10000
#> ************************************************** 100%
#> . . . . Updating 0
#> . Deleting model
#> . 
#> All chains have finished
#> Simulation complete.  Reading coda files...
#> Coda files loaded successfully
#> Finished running the simulation
#> Compiling rjags model and adapting for 1000 iterations...
#> Obtaining DIC samples from 100 iterations...
#> Warning in doTryCatch(return(expr), name, parentenv, handler): Model may not
#> have converged. Max rhat is 1.31941722242303
#> Model fitted successfully with 3 chains and 10000 iterations.
#> Processing fold 3 of 3
#> Single output type detected. Not including output-level random effects in model.
#> Calling 3 simulations using the parallel method...
#> Following the progress of chain 1 (the program will wait for all chains
#> to finish before continuing):
#> Welcome to JAGS 4.3.2 on Tue Dec  9 15:40:56 2025
#> JAGS is free software and comes with ABSOLUTELY NO WARRANTY
#> Loading module: basemod: ok
#> Loading module: bugs: ok
#> . . Reading data file data.txt
#> . Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 71
#>    Unobserved stochastic nodes: 84
#>    Total graph size: 891
#> . Reading parameter file inits1.txt
#> . Initializing model
#> . Adapting 1000
#> -------------------------------------------------| 1000
#> ++++++++++++++++++++++++++++++++++++++++++++++++++ 100%
#> Adaptation successful
#> . Updating 1000
#> -------------------------------------------------| 1000
#> ************************************************** 100%
#> . . . . . . Updating 10000
#> -------------------------------------------------| 10000
#> ************************************************** 100%
#> . . . . Updating 0
#> . Deleting model
#> . 
#> All chains have finished
#> Simulation complete.  Reading coda files...
#> Coda files loaded successfully
#> Finished running the simulation
#> Compiling rjags model and adapting for 1000 iterations...
#> Obtaining DIC samples from 100 iterations...
#> Warning in doTryCatch(return(expr), name, parentenv, handler): Model may not
#> have converged. Max rhat is 1.10597034099288
#> Model fitted successfully with 3 chains and 10000 iterations.

fit <- res |> 
  group_by(fold) |> 
  summarise(rmse = sqrt(mean((observed-mean)^2)),
            mae = mean(abs(observed-mean)))

knitr::kable(fit)
```

| fold |      rmse |       mae |
|-----:|----------:|----------:|
|    1 | 0.4888237 | 0.3982090 |
|    2 | 0.6860582 | 0.5858699 |
|    3 | 0.5730351 | 0.4568500 |

## Reproducing the fitted models installed with the package

There are three pre-fitted models installed with the package:

- [`unitcost()`](../reference/unitcost.md): predicts the cost of a
  single outpatient visit
- [`unitcost_fixed()`](../reference/unitcost_fixed.md): predicts the
  fixed costs associated with a single outpatient visit
- [`unitcost_ohd()`](../reference/unitcost_ohd.md): predicts the fixed
  costs associated with a single outpatient visit

``` r
mod_unit <- unitcost()
mod_unit$fit(seed = 1)
samples <- mod_unit$samples()
DIC <- mod_unit$mcmc_DIC(summarised = FALSE)
saveRDS(samples, "inst/posterior_samples.rds")
saveRDS(DIC, "inst/posterior_samples_dic.rds")

mod_unit_fixed <- unitcost_fixed()
mod_unit_fixed$fit(seed = 1)
samples_fixed <- mod_unit_fixed$samples()
DIC <- mod_unit$mcmc_DIC(summarised = FALSE)
saveRDS(samples_fixed, "inst/posterior_samples_fixed.rds")
saveRDS(DIC_fixed, "inst/posterior_samples_dic_fixed.rds")

mod_unit_ohd <- unitcost_ohd()
mod_unit_ohd$fit(seed = 1)
samples_ohd <- mod_unit_ohd$samples()
DIC_ohd <- mod_unit_ohd$mcmc_DIC(summarised = FALSE)
saveRDS(samples_ohd, "inst/posterior_samples_ohd.rds")
saveRDS(DIC_ohd, "inst/posterior_samples_dic_ohd.rds")
```

Executing the above code will reproduce exactly the posterior samples
installed with this package. The functions
[`unitcost()`](../reference/unitcost.md),
[`unitcost_fixed()`](../reference/unitcost_fixed.md) and
[`unitcost_ohd()`](../reference/unitcost_ohd.md) use the saved
posteriors to load the models without requiring fitting at runtime.
