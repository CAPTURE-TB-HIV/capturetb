# Model fitting

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
#> alpha              3.63999 0.58943 0.0065900      0.0440826
#> beta[1]           -0.07976 0.05250 0.0005870      0.0034858
#> beta[2]            0.04306 0.05614 0.0006277      0.0007997
#> beta[3]           -0.35706 0.09554 0.0010681      0.0021132
#> beta[4]            0.39151 0.13760 0.0015384      0.0021974
#> beta[5]           -0.31515 0.12683 0.0014180      0.0018639
#> sigma              0.56633 0.04131 0.0004619      0.0004793
#> sigma_c            0.55352 0.34696 0.0038792      0.0090506
#> country_effect[1] -0.04368 0.29480 0.0032959      0.0112957
#> country_effect[2]  0.01434 0.30464 0.0034060      0.0121669
#> country_effect[3] -0.47335 0.30591 0.0034202      0.0123174
#> country_effect[4]  0.46327 0.30255 0.0033827      0.0117060
#> country_effect[5]  0.07373 0.31556 0.0035281      0.0141739
#> 
#> 2. Quantiles for each variable:
#> 
#>                       2.5%       25%      50%      75%    97.5%
#> alpha              2.45957  3.248100  3.63830  4.03168  4.80466
#> beta[1]           -0.18062 -0.114780 -0.07994 -0.04503  0.02463
#> beta[2]           -0.06505  0.004846  0.04282  0.07964  0.15365
#> beta[3]           -0.54283 -0.422116 -0.35781 -0.29170 -0.17113
#> beta[4]            0.12058  0.300915  0.39226  0.48136  0.65887
#> beta[5]           -0.56289 -0.399871 -0.31568 -0.22840 -0.06918
#> sigma              0.49281  0.537340  0.56395  0.59285  0.65293
#> sigma_c            0.19362  0.337843  0.46400  0.65480  1.46513
#> country_effect[1] -0.65720 -0.198961 -0.03946  0.12036  0.54653
#> country_effect[2] -0.60567 -0.152113  0.01383  0.18590  0.62753
#> country_effect[3] -1.13906 -0.633423 -0.45924 -0.29217  0.09175
#> country_effect[4] -0.11834  0.292065  0.45010  0.63015  1.10244
#> country_effect[5] -0.54930 -0.100301  0.06681  0.24832  0.71181
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
| alpha               |  178.8888 |
| beta\[1\]           |  228.1807 |
| beta\[2\]           | 5078.3837 |
| beta\[3\]           | 2044.0425 |
| beta\[4\]           | 3964.2402 |
| beta\[5\]           | 4638.6336 |
| sigma               | 7465.0087 |
| sigma_c             | 1563.4148 |
| country_effect\[1\] |  691.8244 |
| country_effect\[2\] |  643.0546 |
| country_effect\[3\] |  693.5719 |
| country_effect\[4\] |  688.8301 |
| country_effect\[5\] |  547.6972 |

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
#> 1           | 3.47 | [2.26, 4.63]
#> 2           | 2.67 | [1.52, 3.85]
#> 3           | 2.09 | [0.90, 3.24]
#> 4           | 1.86 | [0.66, 3.03]
#> 5           | 2.26 | [1.13, 3.41]
#> 6           | 2.28 | [1.12, 3.41]

# Various measures of fit
performance <- model$performance(scale = "log")
knitr::kable(performance)
```

|       mae |      rmse | ci_coverage | median_ci | bayesian_r2 |
|----------:|----------:|------------:|----------:|------------:|
| 0.4350405 | 0.5378033 |   0.9716981 |  2.333307 |   0.4969209 |

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
| Ethiopia    | 6.866966 | 9.348032 |       0.9200000 |        32.83679 |      0.3955880 |
| Georgia     | 6.812158 | 7.944749 |       1.0000000 |        53.92191 |      0.4416998 |
| India       | 3.932385 | 5.029157 |       1.0000000 |        20.26792 |      0.3551333 |
| Kenya       | 7.048425 | 9.401277 |       1.0000000 |        38.63368 |      0.4324251 |
| Philippines | 4.496973 | 5.485710 |       0.9583333 |        22.87324 |      0.4837559 |

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
#> Welcome to JAGS 4.3.2 on Tue Dec  9 16:27:24 2025
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
#> Welcome to JAGS 4.3.2 on Tue Dec  9 16:27:25 2025
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
#> Welcome to JAGS 4.3.2 on Tue Dec  9 16:27:26 2025
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
|    1 | 0.6070211 | 0.4606934 |
|    2 | 0.5744729 | 0.4648416 |
|    3 | 0.7224284 | 0.5981636 |

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
