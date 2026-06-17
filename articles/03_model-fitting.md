# Model fitting

``` r

library(ggplot2)
library(gridExtra)
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

data <- capturetb::get_data(output_name = "op_diagnosticvisit") 
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
#>                        Mean      SD  Naive SE Time-series SE
#> alpha              3.717160 0.57903 0.0064737      0.0410932
#> beta[1]           -0.086912 0.05138 0.0005744      0.0034748
#> beta[2]            0.043977 0.05650 0.0006317      0.0009012
#> beta[3]           -0.354207 0.09505 0.0010627      0.0021873
#> beta[4]            0.393282 0.13378 0.0014957      0.0021024
#> beta[5]           -0.314932 0.12699 0.0014198      0.0017950
#> sigma              0.565475 0.04104 0.0004588      0.0005148
#> sigma_c            0.558653 0.38274 0.0042791      0.0103999
#> country_effect[1] -0.047371 0.31034 0.0034697      0.0128245
#> country_effect[2]  0.009332 0.31845 0.0035603      0.0133316
#> country_effect[3] -0.476962 0.31498 0.0035215      0.0123014
#> country_effect[4]  0.454157 0.31329 0.0035026      0.0124653
#> country_effect[5]  0.061012 0.32196 0.0035996      0.0132944
#> 
#> 2. Quantiles for each variable:
#> 
#>                      2.5%       25%       50%      75%    97.5%
#> alpha              2.5563  3.346355  3.718885  4.09517  4.83860
#> beta[1]           -0.1894 -0.120907 -0.087129 -0.05319  0.01491
#> beta[2]           -0.0667  0.006373  0.044162  0.08153  0.15536
#> beta[3]           -0.5384 -0.417883 -0.355298 -0.29103 -0.16502
#> beta[4]            0.1357  0.305478  0.392502  0.48239  0.65814
#> beta[5]           -0.5630 -0.400325 -0.315263 -0.22944 -0.06745
#> sigma              0.4922  0.537341  0.562751  0.59137  0.65130
#> sigma_c            0.1962  0.335095  0.458472  0.64956  1.57325
#> country_effect[1] -0.6583 -0.211096 -0.051913  0.11077  0.57930
#> country_effect[2] -0.6221 -0.161646  0.006796  0.17578  0.66170
#> country_effect[3] -1.1365 -0.644159 -0.471064 -0.30077  0.11619
#> country_effect[4] -0.1261  0.276733  0.437548  0.61106  1.12765
#> country_effect[5] -0.5553 -0.116087  0.053115  0.22570  0.73883
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
| alpha               |  198.5105 |
| beta\[1\]           |  218.8436 |
| beta\[2\]           | 3929.2337 |
| beta\[3\]           | 1935.5425 |
| beta\[4\]           | 4048.9076 |
| beta\[5\]           | 5007.1738 |
| sigma               | 6637.3224 |
| sigma_c             | 1571.9495 |
| country_effect\[1\] |  608.9179 |
| country_effect\[2\] |  590.0623 |
| country_effect\[3\] |  690.1791 |
| country_effect\[4\] |  652.2322 |
| country_effect\[5\] |  598.4795 |

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
#> 1           | 3.47 | [2.25, 4.66]
#> 2           | 2.66 | [1.49, 3.83]
#> 3           | 2.08 | [0.91, 3.27]
#> 4           | 1.83 | [0.66, 2.99]
#> 5           | 2.27 | [1.11, 3.41]
#> 6           | 2.28 | [1.17, 3.42]

# Various measures of fit
performance <- model$performance(scale = "log")
knitr::kable(performance)
```

|       mae |      rmse | ci_coverage | median_ci | bayesian_r2 |
|----------:|----------:|------------:|----------:|------------:|
| 0.4342554 | 0.5366687 |   0.9716981 |  2.317334 |   0.4981934 |

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
#> Warning in private$.predict(dat, include_epsilon = FALSE, conditional = TRUE):
#> conditional = TRUE has no effect when there is only one output type
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
| Ethiopia    | 6.915761 | 9.381640 |       0.9200000 |        32.65935 |      0.3962081 |
| Georgia     | 6.913585 | 8.056192 |       1.0000000 |        52.79230 |      0.4411096 |
| India       | 3.904838 | 4.976026 |       1.0000000 |        20.57488 |      0.3572328 |
| Kenya       | 7.133080 | 9.480828 |       1.0000000 |        37.98736 |      0.4325622 |
| Philippines | 4.526629 | 5.507942 |       0.9583333 |        22.58626 |      0.4853763 |

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
#> Welcome to JAGS 4.3.2 on Wed Jun 17 11:19:20 2026
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
#> have converged. Max rhat is 1.11280296443821
#> Model fitted successfully with 3 chains and 10000 iterations.
#> Processing fold 2 of 3
#> Single output type detected. Not including output-level random effects in model.
#> Calling 3 simulations using the parallel method...
#> Following the progress of chain 1 (the program will wait for all chains
#> to finish before continuing):
#> Welcome to JAGS 4.3.2 on Wed Jun 17 11:19:21 2026
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
#> Model fitted successfully with 3 chains and 10000 iterations.
#> Processing fold 3 of 3
#> Single output type detected. Not including output-level random effects in model.
#> Calling 3 simulations using the parallel method...
#> Following the progress of chain 1 (the program will wait for all chains
#> to finish before continuing):
#> Welcome to JAGS 4.3.2 on Wed Jun 17 11:19:22 2026
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
#> have converged. Max rhat is 1.10592803272995
#> Model fitted successfully with 3 chains and 10000 iterations.

fit <- res |> 
  group_by(fold) |> 
  summarise(rmse = sqrt(mean((observed-mean)^2)),
            mae = mean(abs(observed-mean)))

knitr::kable(fit)
```

| fold |      rmse |       mae |
|-----:|----------:|----------:|
|    1 | 0.7334447 | 0.6487774 |
|    2 | 0.6232872 | 0.5153740 |
|    3 | 0.4944178 | 0.3698610 |

## Reproducing the fitted models installed with the package

There are three pre-fitted models installed with the package:

- [`unitcost()`](../reference/unitcost.md): predicts the cost of a
  single outpatient visit
- [`unitcost_fixed()`](../reference/unitcost_fixed.md): predicts the
  fixed costs associated with a single outpatient visit
- [`unitcost_ohd()`](../reference/unitcost_ohd.md): predicts the fixed
  costs associated with a single outpatient visit

``` r


# economic cost models
mod_unit <- unitcost()
mod_unit$fit(seed = 1)
samples <- mod_unit$samples()
DIC <- mod_unit$mcmc_DIC(summarised = FALSE)
saveRDS(samples, "inst/econ/posterior_samples.rds")
saveRDS(DIC, "inst/econ/posterior_samples_dic.rds")

mod_unit_fixed <- unitcost_fixed()
mod_unit_fixed$fit(seed = 1)
samples_fixed <- mod_unit_fixed$samples()
DIC_fixed <- mod_unit_fixed$mcmc_DIC(summarised = FALSE)
saveRDS(samples_fixed, "inst/econ/posterior_samples_fixed.rds")
saveRDS(DIC_fixed, "inst/econ/posterior_samples_dic_fixed.rds")

mod_unit_ohd <- unitcost_ohd()
mod_unit_ohd$fit(seed = 1)
samples_ohd <- mod_unit_ohd$samples()
DIC_ohd <- mod_unit_ohd$mcmc_DIC(summarised = FALSE)
saveRDS(samples_ohd, "inst/econ/posterior_samples_ohd.rds")
saveRDS(DIC_ohd, "inst/econ/posterior_samples_dic_ohd.rds")
```

Executing the above code will reproduce exactly the posterior samples
installed with this package. The functions
[`unitcost()`](../reference/unitcost.md),
[`unitcost_fixed()`](../reference/unitcost_fixed.md) and
[`unitcost_ohd()`](../reference/unitcost_ohd.md) use the saved
posteriors to load the models without requiring fitting at runtime.
