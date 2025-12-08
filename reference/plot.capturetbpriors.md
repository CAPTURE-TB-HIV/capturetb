# Plot prior distribution from a `capturetbpriors` object

Plots the distribution of a specified prior from a \`capturetbpriors“
object.

## Usage

``` r
# S3 method for class 'capturetbpriors'
plot(x, ..., par = "alpha")
```

## Arguments

- x:

  Object of class 'capturetbpriors'. See
  [`capturetb_priors()`](capturetb_priors.md).

- ...:

  Further arguments passed to the method.

- par:

  Character. Name of the parameter to plot. ("alpha", "sigma",
  "sigma_c", "beta\[1\]", ...). Default is "alpha".

## Examples

``` r
mod <- unitcost()
#> Multiple outputs detected. Including output-level random effects in model.
plot(mod$priors())
```
