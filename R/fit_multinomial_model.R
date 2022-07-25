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

  dat_mod <- dat_prepped |>
    mutate(swab = as.numeric(sample_type == "Rectal"),
           gender_age = as.factor(if_else(sample_type == "Rectal", paste(gender, age, sep = "-"), "NA")))

  frm =  ~ s(day, bs = "bs", k = 12, by = sample_type) + s(day, gender_age, bs = "fs", xt = list(bs = "bs"), by = swab) +
           s(day_of_year, bs = "cc", by = sample_type, k = 5) + s(day_of_year, gender_age, bs =  "fs", xt = list(bs = "cc"), by = swab) +
           s(fmi_kg_m2, k = 5, by = dummy_rectal)

  mod <- gam(list(as.formula(paste(c("outcome", as.character(frm)), collapse = " ")), frm, frm),
    data = dat_mod,
    knots = list(day_of_year = c(0.5, 365.5)),
    family = multinom(K = 3),
    method = "REML",
    control = list(trace=interactive()))

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
