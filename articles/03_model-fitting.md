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
#>                       Mean      SD  Naive SE Time-series SE
#> alpha              3.64732 0.58690 0.0065618      0.0418910
#> beta[1]           -0.08082 0.05160 0.0005769      0.0034684
#> beta[2]            0.04367 0.05565 0.0006222      0.0008164
#> beta[3]           -0.35475 0.09618 0.0010753      0.0021726
#> beta[4]            0.39285 0.13811 0.0015441      0.0021634
#> beta[5]           -0.31537 0.12742 0.0014246      0.0018725
#> sigma              0.56615 0.04125 0.0004612      0.0004910
#> sigma_c            0.56527 0.37960 0.0042441      0.0119112
#> country_effect[1] -0.04129 0.31474 0.0035188      0.0140501
#> country_effect[2]  0.01654 0.32562 0.0036406      0.0146034
#> country_effect[3] -0.47505 0.32625 0.0036475      0.0138537
#> country_effect[4]  0.46153 0.32038 0.0035820      0.0138717
#> country_effect[5]  0.07168 0.32982 0.0036875      0.0159557
#> 
#> 2. Quantiles for each variable:
#> 
#>                       2.5%       25%      50%      75%    97.5%
#> alpha              2.47531  3.272015  3.64719  4.03701  4.76653
#> beta[1]           -0.18300 -0.116423 -0.07968 -0.04422  0.01623
#> beta[2]           -0.06593  0.006477  0.04404  0.08072  0.15171
#> beta[3]           -0.54352 -0.420015 -0.35574 -0.29034 -0.16230
#> beta[4]            0.12412  0.298965  0.39228  0.48718  0.66483
#> beta[5]           -0.56032 -0.401883 -0.31783 -0.22871 -0.06167
#> sigma              0.49253  0.537141  0.56323  0.59266  0.65425
#> sigma_c            0.19458  0.338421  0.46550  0.67444  1.52860
#> country_effect[1] -0.69312 -0.211418 -0.04512  0.11831  0.64386
#> country_effect[2] -0.64577 -0.163750  0.01503  0.18612  0.73225
#> country_effect[3] -1.16253 -0.650566 -0.46613 -0.29650  0.18738
#> country_effect[4] -0.14815  0.274859  0.44263  0.61973  1.20165
#> country_effect[5] -0.57692 -0.113574  0.06290  0.24221  0.78122
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
| alpha               |  198.6499 |
| beta\[1\]           |  224.6304 |
| beta\[2\]           | 5109.1969 |
| beta\[3\]           | 1979.7448 |
| beta\[4\]           | 4090.6761 |
| beta\[5\]           | 4666.8445 |
| sigma               | 7158.1747 |
| sigma_c             | 1052.2727 |
| country_effect\[1\] |  536.6810 |
| country_effect\[2\] |  532.3574 |
| country_effect\[3\] |  578.6795 |
| country_effect\[4\] |  557.0588 |
| country_effect\[5\] |  453.5466 |

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
#> 1           | 3.46 | [2.22, 4.65]
#> 2           | 2.66 | [1.51, 3.82]
#> 3           | 2.09 | [0.92, 3.27]
#> 4           | 1.84 | [0.70, 3.01]
#> 5           | 2.27 | [1.14, 3.40]
#> 6           | 2.28 | [1.14, 3.42]

# Various measures of fit
performance <- model$performance(scale = "log")
knitr::kable(performance)
```

|      mae |      rmse | ci_coverage | median_ci | bayesian_r2 |
|---------:|----------:|------------:|----------:|------------:|
| 0.435237 | 0.5383155 |   0.9716981 |  2.325109 |   0.4970555 |

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
| Ethiopia    | 6.893954 | 9.369819 |       0.9200000 |        33.98021 |      0.3936966 |
| Georgia     | 6.872686 | 8.068657 |       1.0000000 |        54.17876 |      0.4400591 |
| India       | 3.910433 | 4.990119 |       1.0000000 |        21.25298 |      0.3567820 |
| Kenya       | 7.060759 | 9.412413 |       1.0000000 |        38.16630 |      0.4319562 |
| Philippines | 4.546214 | 5.559223 |       0.9583333 |        23.04655 |      0.4811488 |

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
#> Welcome to JAGS 4.3.2 on Wed Jun 17 09:22:31 2026
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
#> Welcome to JAGS 4.3.2 on Wed Jun 17 09:22:32 2026
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
#> Welcome to JAGS 4.3.2 on Wed Jun 17 09:22:33 2026
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
|    1 | 0.6066659 | 0.4871743 |
|    2 | 0.5667321 | 0.4617924 |
|    3 | 0.7207235 | 0.5818368 |

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
