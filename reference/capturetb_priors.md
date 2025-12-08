# Create prior distributions for a model.

Constructs a list of prior distributions for model parameters. This
function validates the input arguments and returns a list with class
`capturetbpriors`. See
[`vignette("03_model-fitting", package = "capturetb")`](../articles/03_model-fitting.md)
for details of the model structure.

## Usage

``` r
capturetb_priors(
  alpha.mean = 0,
  alpha.precision = 0.01,
  sigma.scale = 10,
  sigma_c.scale = 10,
  sigma_f.scale = 10,
  sigma_v.scale = 10,
  beta.mean = 0,
  beta.precision = 0.01
)
```

## Arguments

- alpha.mean:

  Numeric scalar. Mean of the prior for the intercept `alpha`. Default
  is 0.

- alpha.precision:

  Numeric scalar. Precision (inverse variance) of the prior for the
  intercept \`alpha“. Default is 0.01.

- sigma.scale:

  Numeric scalar. Scale parameter for Half-Cauchy prior on `sigma`.
  Default is 1.

- sigma_c.scale:

  Numeric scalar. Scale parameter for Half-Cauchy prior on `sigma_c`.
  Default is 1.

- sigma_f.scale:

  Numeric scalar. Scale parameter for Half-Student-t with 3 degrees of
  freedom prior on `sigma_f`. Default is 1.

- sigma_v.scale:

  Numeric scalar. Scale parameter for Half-Cauchy prior on `sigma_v`.
  Default is 1.

- beta.mean:

  Numeric vector. Means of the priors for the `beta` coefficients of
  fixed effects.

- beta.precision:

  Numeric vector. Precision of the priors for the `beta` coefficients of
  fixed effects.

## Value

A list of prior parameters with class `capturetbpriors`.
