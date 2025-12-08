# Prepare covariates for prediction

Centers numeric covariates as required by the model.

## Usage

``` r
prepare_covariates(raw, model)
```

## Arguments

- raw:

  List or data frame of raw covariate values.

- model:

  A [`capturetb::JAGSModel`](JAGSModel.md) object.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with class 'capturetbdata' suitable for input
[`JAGSModel`](JAGSModel.md)'s `$predict()`, and `$performance()`
methods.

## Examples

``` r
mod <- unitcost()
#> Multiple outputs detected. Including output-level random effects in model.
prepare_covariates(
  raw = list(
    n_services = 5,
    public = 1,
    urban = 0,
    primary = 0,
    secondary = 1,
    tertiary = 0,
    log_ID_p_bldgspace = 1,
    logVisits = 6.9,
    logVisitsPP_TB = -1.29,
    fc_country = "Ethiopia",
    output = "op_treatmentvisit"
  ),
  model = mod
)
#> # A tibble: 1 × 11
#>   n_services[,1] public urban primary secondary tertiary log_ID_p_bldgspace[,1]
#>            <dbl>  <dbl> <dbl>   <dbl>     <dbl>    <dbl>                  <dbl>
#> 1         -0.255      1     0       0         1        0                  -3.25
#> # ℹ 4 more variables: logVisits <dbl[,1]>, logVisitsPP_TB <dbl[,1]>,
#> #   fc_country <chr>, output <chr>
```
