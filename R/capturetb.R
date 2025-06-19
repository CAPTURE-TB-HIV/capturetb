#' CaptureTB total unit cost model
#'
#' This function loads a \code{MixedEffects} model object
#' fitted using default covariates and priors, with
#' a total of 30,000 posterior samples. This can be used to
#' predict unit costs for new inputs via the predict method.
#'
#' @return An object of class \code{MixedEffects}.
#' @examples
#' mod <- unitcost()
#' \dontrun{
#' mod$predict(new_data)
#' }
#' @export
unitcost <- function() {
    samples <- readRDS(system.file("posterior_samples.rds",
        package = "capturetb"
    ))
    mod <- MixedEffects$new()
    mod$.__enclos_env__$private$.samples <- samples
    mod
}
