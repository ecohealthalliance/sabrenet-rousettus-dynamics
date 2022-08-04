fit_multinomial_model <- function(dat_prepped, nthreads = 8, n_blas_threads = 16) {

  library(RhpcBLASctl)
  old_nthreads <- RhpcBLASctl::omp_get_num_procs()
  old_n_blas_threads <- RhpcBLASctl::blas_get_num_procs()
  RhpcBLASctl::blas_set_num_threads(n_blas_threads)
  RhpcBLASctl::omp_set_num_threads(nthreads)
  on.exit({
    RhpcBLASctl::omp_set_num_threads(old_nthreads)
    RhpcBLASctl::blas_set_num_threads(old_n_blas_threads)
  })

   frm = ~ s(sample_type, bs = "re", by = dummy_rectal) +
           s(day, bs = "tp", k = 5, by = sample_type, m = 2) +  # Overall trend, split by fecal and swab
           s(day, gender_age, bs = "fs", k = 5, xt = list(bs = "tp"), by = dummy_rectal, m = 2) + # Overall trend deviance for different bat groups, swab only
           s(day_of_year, bs = "cc", by = sample_type, k = 5, m = 2) + # Seasonal effect, split by fecal/swab
           s(day_of_year, gender_age, bs =  "fs", xt = list(bs = "cc"), k = 5, by = dummy_rectal, m = 2) + # Seasonal deviance for different bat groups, swab only
           s(fmi_kg_m2, k = 5, bs = "tp", by = dummy_rectal) + # Effect of bat FMI, swab only
           s(reproductive_condition, bs = "re", by = dummy_repro) + # Effect of lactation/pregnancy/scrotal, reproductively active adults only
           s(frac_subadult, bs = "tp", k = 5, by = dummy_any_rectal) + # Effect of population subadult fraction
           s(frac_subadult, sample_type, bs = "fs", k = 5, xt = list(bs = "tp"), by = dummy_any_rectal, m = 2)  # Difference in effect of subadult fraction on fecal and swab

   # a separate formula for the rarer outcome 3
   frm3 <- ~ s(sample_type, bs = "re", by = dummy_rectal) +
     s(day, bs = "tp", k = 5, by = sample_type, m = 2) +  # Overall trend, split by fecal and swab
    # s(day, gender_age, bs = "fs", k = 5, xt = list(bs = "tp"), by = dummy_rectal, m = 2) # Overall trend deviance for different bat groups, swab only
     s(day_of_year, bs = "cc", by = sample_type, k = 5, m = 2) # Seasonal effect, split by fecal/swab
    # s(day_of_year, gender_age, bs =  "fs", xt = list(bs = "cc"), k = 5, by = dummy_rectal, m = 2) + # Seasonal deviance for different bat groups, swab only
    # s(fmi_kg_m2, k = 5, bs = "tp", by = dummy_rectal) + # Effect of bat FMI, swab only
    # s(reproductive_condition, bs = "re", by = dummy_repro) + # Effect of lactation/pregnancy/scrotal, reproductively active adults only
    # s(frac_subadult, bs = "tp", k = 5, by = dummy_any_rectal) + # Effect of population subadult fraction
    # s(frac_subadult, sample_type, bs = "fs", k = 5, xt = list(bs = "tp"), by = dummy_any_rectal, m = 2)  # Difference in effect of subadult fraction on fecal and swab

  multinomial_model <- gam(
    list(as.formula(paste(c("outcome", as.character(frm)), collapse = " ")), frm, frm3),
    data = dat_prepped,
    knots = list(day_of_year = c(0.5, 365.5)),
    family = multinom(K = 3),
    method = "REML",
    control = list(
      nthreads = nthreads,
      trace=interactive()
      )
    )
  multinomial_model

}

sample_gam_posterior <- function(multinomial_model, chains = 4, cores = min(chains, parallel::detectCores()), ...) {
  samps <-
    purrr::transpose(parallel::mclapply(X = seq_len(chains),
                                        FUN = function(X) {
                                          post <- gam.mh(multinomial_model, ...)
                                          #post <- gam.mh(multinomial_model, burn = 5000, ns = 15000, thin = 10, rw.scale = 0.05)
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
  posterior_stats <- tibble(
    parameter = dimnames(gam_posterior)[[3]],
    Rhat = apply(gam_posterior, 3, rstan::Rhat),
    ess_bulk = apply(gam_posterior, 3, rstan::ess_bulk),
    ess_tail = apply(gam_posterior, 3, rstan::ess_tail)
  )

  posterior_stats
}
