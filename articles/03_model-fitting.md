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
#>                        Mean      SD  Naive SE Time-series SE
#> alpha              3.693891 0.55694 0.0062268      0.0390390
#> beta[1]           -0.083795 0.05107 0.0005709      0.0035454
#> beta[2]            0.042847 0.05509 0.0006159      0.0007329
#> beta[3]           -0.352723 0.09513 0.0010636      0.0021314
#> beta[4]            0.391909 0.13599 0.0015204      0.0021652
#> beta[5]           -0.314346 0.12703 0.0014202      0.0017662
#> sigma              0.565431 0.04189 0.0004683      0.0005217
#> sigma_c            0.535298 0.32108 0.0035898      0.0090942
#> country_effect[1] -0.058305 0.28796 0.0032195      0.0110588
#> country_effect[2] -0.001075 0.29899 0.0033428      0.0112628
#> country_effect[3] -0.485822 0.29954 0.0033490      0.0117055
#> country_effect[4]  0.441118 0.29175 0.0032619      0.0113584
#> country_effect[5]  0.050093 0.30284 0.0033859      0.0130013
#> 
#> 2. Quantiles for each variable:
#> 
#>                       2.5%       25%       50%      75%    97.5%
#> alpha              2.56478  3.330642  3.713870  4.05971  4.77721
#> beta[1]           -0.17793 -0.119204 -0.085343 -0.05168  0.02454
#> beta[2]           -0.06464  0.006296  0.043334  0.07984  0.15156
#> beta[3]           -0.53848 -0.416856 -0.353313 -0.28987 -0.16382
#> beta[4]            0.12383  0.301565  0.391743  0.48080  0.66406
#> beta[5]           -0.56572 -0.400281 -0.314143 -0.22804 -0.06398
#> sigma              0.49022  0.536776  0.562353  0.59156  0.65510
#> sigma_c            0.19515  0.334823  0.451118  0.63488  1.40718
#> country_effect[1] -0.66321 -0.207631 -0.047928  0.10368  0.49410
#> country_effect[2] -0.62451 -0.161950  0.006757  0.16977  0.57068
#> country_effect[3] -1.12992 -0.640549 -0.467450 -0.30586  0.04999
#> country_effect[4] -0.12928  0.277039  0.435279  0.60468  1.02946
#> country_effect[5] -0.56720 -0.121470  0.051878  0.23369  0.64394
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
| alpha               |  205.3075 |
| beta\[1\]           |  208.0827 |
| beta\[2\]           | 5665.2712 |
| beta\[3\]           | 2041.2874 |
| beta\[4\]           | 3946.0217 |
| beta\[5\]           | 5192.5507 |
| sigma               | 6464.4656 |
| sigma_c             | 1849.6510 |
| country_effect\[1\] |  824.1283 |
| country_effect\[2\] |  773.3782 |
| country_effect\[3\] |  793.1910 |
| country_effect\[4\] |  793.2115 |
| country_effect\[5\] |  744.5069 |

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
#> 1           | 3.46 | [2.25, 4.68]
#> 2           | 2.67 | [1.49, 3.81]
#> 3           | 2.10 | [0.92, 3.27]
#> 4           | 1.84 | [0.66, 3.00]
#> 5           | 2.27 | [1.11, 3.43]
#> 6           | 2.27 | [1.11, 3.39]

# Various measures of fit
performance <- model$performance(scale = "log")
knitr::kable(performance)
```

|       mae |     rmse | ci_coverage | median_ci | bayesian_r2 |
|----------:|---------:|------------:|----------:|------------:|
| 0.4339115 | 0.537164 |   0.9716981 |  2.326941 |   0.4969562 |

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
| Ethiopia    | 6.952096 | 9.424638 |       0.9200000 |        33.34361 |      0.3939261 |
| Georgia     | 6.929865 | 8.096010 |       1.0000000 |        53.46207 |      0.4413334 |
| India       | 3.915063 | 4.983460 |       1.0000000 |        20.52548 |      0.3575759 |
| Kenya       | 7.052171 | 9.450824 |       1.0000000 |        36.24918 |      0.4308435 |
| Philippines | 4.523624 | 5.493722 |       0.9583333 |        22.70544 |      0.4821724 |

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
#> Welcome to JAGS 4.3.2 on Mon Dec  8 11:39:36 2025
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
#> Model fitted successfully with 3 chains and 10000 iterations.
#> Processing fold 2 of 3
#> Single output type detected. Not including output-level random effects in model.
#> Calling 3 simulations using the parallel method...
#> Following the progress of chain 1 (the program will wait for all chains
#> to finish before continuing):
#> Welcome to JAGS 4.3.2 on Mon Dec  8 11:39:37 2025
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
#> Welcome to JAGS 4.3.2 on Mon Dec  8 11:39:38 2025
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

fit <- res |> 
  group_by(fold) |> 
  summarise(rmse = sqrt(mean((observed-mean)^2)),
            mae = mean(abs(observed-mean)))

knitr::kable(fit)
```

| fold |      rmse |       mae |
|-----:|----------:|----------:|
|    1 | 0.6168572 | 0.4972917 |
|    2 | 0.5699903 | 0.4633673 |
|    3 | 0.6013164 | 0.4898108 |

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
