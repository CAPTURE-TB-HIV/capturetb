# capturetb [![R-CMD-check](https://github.com/CAPTURE-TB-HIV/capturetb/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CAPTURE-TB-HIV/capturetb/actions/workflows/R-CMD-check.yaml) [![codecov](https://codecov.io/gh/CAPTURE-TB-HIV/capturetb/graph/badge.svg?token=LC3NJPM0SY)](https://codecov.io/gh/CAPTURE-TB-HIV/capturetb) ![Work in Progress](https://img.shields.io/badge/status-work--in--progress-yellow)

## Installation

```r
remotes::install_github("CAPTURE-TB-HIV/capturetb")
```

## Basic usage

To predict the unitcost of one outpatient visit given facility characteristics:

```r
# Load model
model <- capturetb::unitcost()

# View covariates
covariates <- model$covariates()
print(covariates)
[1] "public"             "urban"              "primary"           
[4] "secondary"          "tertiary"           "n_services"        
[7] "log_ID_p_bldgspace" "logVisits"          "logVisitsPP_TB"     

# Generate predictions from posterior
inputs <- list(
  log_ID_p_bldgspace = 1,
  logVisits = 6.9, 
  logVisitsPP_TB = -1.29, 
  primary = TRUE,
  secondary = FALSE, 
  tertiary = FALSE,
  urban = FALSE, 
  public = TRUE,
  n_services = 3,
  output = "op_treatmentvisit",
  fc_country = "Ethiopia"
)
prepared_inputs <- prepare_covariates(inputs, model)
pred <- model$predict(
	prepared_inputs,
	scale = "natural",
	summarised = TRUE)

# Expected unit cost is mean prediction
expected_unit_cost <- pred$Mean
print(expected_unit_cost)
[1] 4.08444
```

## Advanced usage

If fitting models, you will also need to install [JAGS](https://sourceforge.net/projects/mcmc-jags/) and the [runjags](https://cran.r-project.org/web/packages/runjags/index.html) package.

See vignettes for [full documentation](https://capturetb-hiv.github.io/capturetb).

## Testing

To run tests:

```r
devtools::load_all()
devtools::test()
```

To build pkgdown documentation locally:

```r
devtools::load_all()
pkgdown::build_site()
```

Files will appear in the `docs/` folder.
