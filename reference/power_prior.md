# Constructs a power prior for the effect of a covariate based on a historic mean, upper and lower estimates, and a scaling factor.

Constructs a power prior for the effect of a covariate based on a
historic mean, upper and lower estimates, and a scaling factor.

## Usage

``` r
power_prior(mu, upper, lower, a0)
```

## Arguments

- mu:

  Mean of the past estimate.

- upper:

  Upper bound of the 95% credible interval for the past estimate.

- lower:

  Lower bound of the 95% credible interval for the past estimate.

- a0:

  Scaling factor for the power prior.
