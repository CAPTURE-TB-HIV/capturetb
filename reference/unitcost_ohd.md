# CaptureTB outpatient visit overhead costs model

This function loads a [`JAGSModel`](JAGSModel.md) model object fitted
using default covariates and priors, with a total of 30,000 posterior
samples. This can be used to predict the overhead costs per outpatient
visit at a given facility or facilities via the predict method.

## Usage

``` r
unitcost_ohd()
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
mod <- unitcost_ohd()
#> Single output type detected. Not including output-level random effects in model.
new_data <- list(
  log_ID_p_bldgspace = 1,
  logVisits = 6.9,
  logVisitsPP_TB = -1.29,
  primary = TRUE,
  secondary = FALSE,
  tertiary = FALSE,
  urban = FALSE,
  public = TRUE,
  n_services = 3,
  fc_country = "Ethiopia"
)
new_covariates <- prepare_covariates(new_data, mod)
mod$predict(new_covariates, summarised = TRUE)
#> Summary of Posterior Distribution
#> 
#> Observation | Mean |       95% CI
#> ---------------------------------
#> 1           | 2.80 | [1.19, 4.43]
```
