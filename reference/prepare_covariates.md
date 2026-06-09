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
    public = 1,
    urban = 0,
    healthcentre = 0,
    primary = 0,
    secondary = 1,
    tertiary = 0,
    logVisits = 6.9,
    fc_country = "Ethiopia",
    output = "op_treatmentvisit"
  ),
  model = mod
)
#> # A tibble: 1 × 9
#>   public urban healthcentre primary secondary tertiary logVisits[,1] fc_country
#>    <dbl> <dbl>        <dbl>   <dbl>     <dbl>    <dbl>         <dbl> <chr>     
#> 1      1     0            0       0         1        0         -3.67 Ethiopia  
#> # ℹ 1 more variable: output <chr>
```
