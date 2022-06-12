#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dat_prepped
#' @return
#' @author 'Noam Ross'
#' @export
fit_multinomial_model <- function(dat_prepped) {

  mod <- gam(list(
    outcome ~ s(day, bs = "ad", k = 15, m = 3, by = sample_type) + s(day_of_year, bs = "cc", by = sample_type, k = 5) + s(fmi_kg_m2, k = 5, by = dummy_rectal),
            ~ s(day, bs = "ad", k = 15, m = 3, by = sample_type) + s(day_of_year, bs = "cc", by = sample_type, k = 5) + s(fmi_kg_m2, k = 5, by = dummy_rectal),
            ~ s(day, bs = "ad", k = 15, m = 3, by = sample_type) + s(day_of_year, bs = "cc", by = sample_type, k = 5) + s(fmi_kg_m2, k = 5, by = dummy_rectal)
    ),
    data = dat_prepped,
    knots = list(day_of_year = c(0.5, 365.5)),
    family = multinom(K = 3),
    method = "REML")

  mod

}

sample_gam_posterior <- function(mod, chains = 4, cores = getOption("mc.cores", 2L), ...) {
  samps <-
    purrr::transpose(parallel::mclapply(X = seq_len(chains),
                                        FUN = function(X) {
                                          post <- gam.mh(mod, ...)
                                          post
                                        },
                                        mc.cores = cores
    ))
  gam_posterior <-
    structure(
      aperm(abind::abind(samps$bs, along = 3), c(1,3,2)),
      rw.accept = unlist(samps$rw.accept),
      accept = unlist(samps$accept)
    )
  dimnames(gam_posterior) <- list(Iteration = NULL, Chain = NULL, Parameter = dimnames(gam_posterior)[[3]])

  gam_posterior

}

calc_posterior_stats <- function(gam_posterior) {
  tibble(
    parameter = dimnames(gam_posterior)[[3]],
    Rhat = apply(gam_posterior, 3, rstan::Rhat),
    ess_bulk = apply(gam_posterior, 3, rstan::ess_bulk),
    ess_tail = apply(gam_posterior, 3, rstan::ess_tail)
  )
}
