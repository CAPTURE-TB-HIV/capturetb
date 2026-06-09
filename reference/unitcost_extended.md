# Extended CaptureTB outpatient visit cost model with more covariates.

This function loads a [`JAGSModel`](JAGSModel.md) model object using
default covariates and priors. This can be used to predict the total
cost of a single outpatient visit at a given facility or facilities via
the predict method. This model is not pre-fitted, so requires JAGS to be
installed to fit and use.

## Usage

``` r
unitcost_extended(cost_type = "ECON")
```

## Arguments

- cost_type:

  One of "ECON" or "FIN". If "ECON", model for economic costs is
  returned. If "FIN", model for financial costs is returned. Default
  "ECON".

## Value

An object of class [`JAGSModel`](JAGSModel.md).

## Details

Note that numerical covariates are centered. When making predictions
with the model, the function
[`prepare_covariates()`](prepare_covariates.md) can be used to transform
raw variables using the correct centering values.

## See also

JAGSModel

## Examples

``` r
if (FALSE) { # \dontrun{
mod <- unitcost_extended()
mod$fit()
} # }
```
