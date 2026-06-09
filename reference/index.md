# Package index

## Predictions

Functions needed to predict costs

- [`prepare_covariates()`](prepare_covariates.md) : Prepare covariates
  for prediction
- [`unitcost()`](unitcost.md) : CaptureTB outpatient visit cost model
- [`unitcost_fixed()`](unitcost_fixed.md) : CaptureTB outpatient visit
  fixed costs model
- [`unitcost_ohd()`](unitcost_ohd.md) : CaptureTB outpatient visit
  overhead costs model
- [`unitcost_extended()`](unitcost_extended.md) : Extended CaptureTB
  outpatient visit cost model with more covariates.
- [`unitcost_fixed_extended()`](unitcost_fixed_extended.md) : Extended
  CaptureTB outpatient visit fixed cost model with more covariates.
- [`unitcost_ohd_extended()`](unitcost_ohd_extended.md) : Extended
  CaptureTB outpatient visit overhead cost model with more covariates.

## Data

Functions for interfacing with ValueTB data

- [`output_groups()`](output_groups.md) : List unique output type groups
  in raw data
- [`outputs()`](outputs.md) : List unique output types in raw data
- [`get_data()`](get_data.md) : Get raw data filtered by output type

## Advanced usage

Functions for fitting models to new data, using different model
structures, or using different priors

- [`JAGSModel`](JAGSModel.md) : JAGSModel R6 Class

- [`capturetb_priors()`](capturetb_priors.md) : Create prior
  distributions for a model.

- [`plot(`*`<capturetbpriors>`*`)`](plot.capturetbpriors.md) :

  Plot prior distribution from a `capturetbpriors` object
