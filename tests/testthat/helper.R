mock_samples <- function(n_sim) {
  smat <- matrix(
    c(
      rep(0, n_sim), # sigma
      rep(1, n_sim), # alpha
      rep(0.01, n_sim), # sigma_c
      rep(0, n_sim), # sigma_f
      rep(0.03, n_sim), # sigma_v
      rep(0.2, n_sim), # beta[1]
      rep(0.3, n_sim), # beta[2]
      rep(0.4, n_sim), # beta[3]
      rep(0.2, n_sim), # country_effect[1]
      rep(0.3, n_sim), # country_effect[2]
      rep(1, n_sim), # output_effect[1]
      rep(2, n_sim) # output_effect[2]
    ),
    nrow = n_sim,
    ncol = 12,
    byrow = FALSE
  )
  colnames(smat) <- c(
    "sigma",
    "alpha",
    "sigma_c",
    "sigma_f",
    "sigma_v",
    paste0("beta[", 1:3, "]"),
    paste0("country_effect[", 1:2, "]"),
    paste0("output_effect[", 1:2, "]")
  )
  fake_samples <- coda::as.mcmc(smat)
  coda::mcmc.list(fake_samples)
}

dat_treatment <- get_data(output_name = "op_treatmentvisit") |>
  dplyr::filter(
    dplyr::if_all(
      dplyr::all_of(c(
        "log_USD_p_bldgspace",
        "logVisits",
        "logVisitsPP_TB",
        "secondary",
        "public"
      )),
      ~ !is.na(.) & !is.nan(.) & is.finite(.)
    )
  )

dat_multioutput <- get_data(output_group = "OP") |>
  dplyr::filter(
    dplyr::if_all(
      dplyr::all_of(c(
        "log_USD_p_bldgspace",
        "logVisits",
        "logVisitsPP_TB",
        "secondary",
        "public"
      )),
      ~ !is.na(.) & !is.nan(.) & is.finite(.)
    )
  )

test_covariates <- c(
  "log_USD_p_bldgspace",
  "logVisits",
  "logVisitsPP_TB",
  "secondary",
  "public"
)
