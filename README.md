# capturetb [![R-CMD-check](https://github.com/CAPTURE-TB-HIV/capturetb/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CAPTURE-TB-HIV/capturetb/actions/workflows/R-CMD-check.yaml) [![codecov](https://codecov.io/gh/CAPTURE-TB-HIV/capturetb/graph/badge.svg?token=LC3NJPM0SY)](https://codecov.io/gh/CAPTURE-TB-HIV/capturetb) ![Work in Progress](https://img.shields.io/badge/status-work--in--progress-yellow)

## Installation

```r
remotes::install_github("CAPTURE-TB-HIV/capturetb")
```

## Basic usage

To predict the unitcost of one outpatient treatment visit given facility characteristics:

```r
# Load model
model <- capturetb::unitcost()

# View covariates
covariates <- model$covariates()
print(covariates)
# [1] "log_USD_p_bldgspace" "logVisits"           "logVisitsPP_TB"        
# [4] "secondary"           "urban"               "public"    

# Generate predictions from posterior
pred <- model$predict(list(
  log_USD_p_bldgspace = 1,
  logVisits = 6.9, 
  logVisitsPP_TB = -1.29, 
	primary = TRUE,
  secondary = FALSE, 
  urban = FALSE, 
  public = TRUE,
	n_services = 3,
	output = "op_treatmentvisit",
  fc_country = "Ethiopia"
), scale = "natural", summarised = TRUE)

# Expected unit cost is mean prediction
expected_unit_cost <- pred$Mean
print(expected_unit_cost)
[1] 8.659947
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
