# CaptureTB outpatient visit cost model

This function loads a [`JAGSModel`](JAGSModel.md) model object fitted
using default covariates and priors, with a total of 30,000 posterior
samples. This can be used to predict the total cost of a single
outpatient visit at a given facility or facilities via the predict
method.

## Usage

``` r
unitcost()
```

## Value

An object of class [`JAGSModel`](JAGSModel.md).

## Details

Note that some covariates are centered. The function
[`prepare_covariates()`](prepare_covariates.md) can be used to transform
raw variables using the correct centering values.

## See also

JAGSModel

## Examples

``` r
mod <- unitcost()
#> Multiple outputs detected. Including output-level random effects in model.
new_data <- list(
  logVisits = 6.9,
  healthcentre = FALSE,
  primary = TRUE,
  secondary = FALSE,
  tertiary = FALSE,
  urban = FALSE,
  public = TRUE,
  fc_country = "Ethiopia",
  output = "op_treatmentvisit"
)
new_covariates <- prepare_covariates(new_data, mod)
mod$predict(new_covariates, summarised = TRUE)
#> Summary of Posterior Distribution
#> 
#> Observation | Mean |       95% CI
#> ---------------------------------
#> 1           | 2.66 | [1.36, 3.95]
```
