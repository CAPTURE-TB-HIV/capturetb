capturetb_total <- function() {
    samples <- readRDS(system.file("posterior_samples.rds",
        package = "capturetb"
    ))
    mod <- MixedEffects$new()
    mod$.__enclos_env__$private$.samples <- samples
    mod
}
