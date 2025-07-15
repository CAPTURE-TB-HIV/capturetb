mock_samples <- function(n_sim) {
  smat <- matrix(
    c(
      rep(0, n_sim), # sigma
      rep(1, n_sim), # mu_alpha
      rep(0.01, n_sim), # sigma_alpha
      rep(0.2, n_sim), # beta[1]
      rep(0.3, n_sim), # beta[2]
      rep(0.4, n_sim), # beta[3]
      rep(2, n_sim), # alpha[1]
      rep(3, n_sim) # alpha[2]
    ),
    nrow = n_sim,
    ncol = 8,
    byrow = FALSE
  )
  colnames(smat) <- c(
    "sigma",
    "mu_alpha", "sigma_alpha",
    paste0("beta[", 1:3, "]"),
    paste0("alpha[", 1:2, "]")
  )
  fake_samples <- coda::as.mcmc(smat)
  coda::mcmc.list(fake_samples)
}

dat_cleaned <- get_data("OP treatment visit") |>
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
  ) |>
  dplyr::group_by(.data$fc_code) |>
  dplyr::slice(1) |>
  dplyr::ungroup()
