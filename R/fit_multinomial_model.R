fit_multinomial_model <- function(dat_prepped, method = "REML", optimizer = "efs", threads = 8, n_blas_threads = 8) {

  library(RhpcBLASctl)
  old_nthreads <- RhpcBLASctl::omp_get_num_procs()
  old_n_blas_threads <- RhpcBLASctl::blas_get_num_procs()
  RhpcBLASctl::blas_set_num_threads(n_blas_threads)
  RhpcBLASctl::omp_set_num_threads(threads)
  on.exit({
    RhpcBLASctl::omp_set_num_threads(old_nthreads)
    RhpcBLASctl::blas_set_num_threads(old_n_blas_threads)
  })

  nthreads = threads
  ncv.threads = threads

  frm <- ~ s(sample_type, bs = "re", by = dummy_rectal) +
    s(day, bs = "tp", k = 5, by = sample_type, m = 2) + # Overall trend, split by fecal and swab
    s(day, gender_age, bs = "fs", k = 5, xt = list(bs = "tp"), by = dummy_rectal, m = 1) + # Overall trend deviance for different bat groups, swab only
    s(day_of_year, bs = "cc", by = sample_type, k = 5, m = 2) + # Seasonal effect, split by fecal/swab
    s(day_of_year, gender_age, bs = "fs", xt = list(bs = "cc"), k = 5, by = dummy_rectal, m = 1) + # Seasonal deviance for different bat groups, swab only
    s(fmi_normalized, k = 5, bs = "tp", by = dummy_rectal) + # Effect of bat FMI, swab only
    s(reproductive_condition, bs = "re", by = dummy_repro) # Effect of lactation/pregnancy/scrotal, reproductively active adults only

  # a separate formula for the rarer outcome 3
  frm3 <- ~ s(sample_type, bs = "re", by = dummy_rectal) +
    s(day, bs = "tp", k = 5, by = sample_type, m = 2) + # Overall trend, split by fecal and swab
    s(day_of_year, bs = "cc", by = sample_type, k = 5, m = 2) # Seasonal effect, split by fecal/swab

  multinomial_model <- gam(
    list(as.formula(paste(c("outcome", as.character(frm)), collapse = " ")), frm, frm3),
    data = dat_prepped,
    knots = list(day_of_year = c(0.5, 365.5)),
    family = multinom(K = 3),
    method = method,
    optimizer = optimizer,
    control = list(
      nthreads = nthreads,
      ncv.threads = ncv.threads,
      trace = TRUE
    )
  )
  multinomial_model
}

fit_multinomial_model_alt <- function(dat_prepped, method = "REML", optimizer = "efs", threads = 8, n_blas_threads = 8) {

  library(RhpcBLASctl)
  old_nthreads <- RhpcBLASctl::omp_get_num_procs()
  old_n_blas_threads <- RhpcBLASctl::blas_get_num_procs()
  RhpcBLASctl::blas_set_num_threads(n_blas_threads)
  RhpcBLASctl::omp_set_num_threads(threads)
  on.exit({
    RhpcBLASctl::omp_set_num_threads(old_nthreads)
    RhpcBLASctl::blas_set_num_threads(old_n_blas_threads)
  })

  nthreads = threads
  ncv.threads = threads

  frm <- ~ s(sample_type, bs = "re", by = dummy_rectal) +
    s(day, bs = "tp", k = 5, by = sample_type, m = 2) + # Overall trend, split by fecal and swab
    s(day_of_year, bs = "cc", by = sample_type, k = 5, m = 2) + # Seasonal effect, split by fecal/swab
    s(gender_age, bs = "re", by = dummy_rectal) +
    s(fmi_normalized, k = 5, bs = "tp", by = dummy_rectal) + # Effect of bat FMI, swab only
    s(reproductive_condition, bs = "re", by = dummy_repro) # Effect of lactation/pregnancy/scrotal, reproductively active adults only

  # a separate formula for the rarer outcome 3
  frm3 <- ~ s(sample_type, bs = "re", by = dummy_rectal) +
    s(day, bs = "tp", k = 5, by = sample_type, m = 2) + # Overall trend, split by fecal and swab
    s(day_of_year, bs = "cc", by = sample_type, k = 5, m = 2) # Seasonal effect, split by fecal/swab

  multinomial_model <- gam(
    list(as.formula(paste(c("outcome", as.character(frm)), collapse = " ")), frm, frm3),
    data = dat_prepped,
    knots = list(day_of_year = c(0.5, 365.5)),
    family = multinom(K = 3),
    method = method,
    optimizer = optimizer,
    control = list(
      nthreads = nthreads,
      ncv.threads = ncv.threads,
      trace = TRUE
    )
  )
  multinomial_model
}

sample_gam_posterior <- function(multinomial_model,
                                 chains = 4,
                                 cores = min(chains, parallel::detectCores()), ...) {
  Vp <- multinomial_model$Vp
  eps <- .Machine$double.xmin
  while(inherits(try(chol(Vp), silent = TRUE), "try-error")) {
    Vp <- Vp + eps*diag(nrow(Vp))
    eps <- eps*2
  }
  multinomial_model$Vp <- Vp
  samps <-
    purrr::transpose(parallel::mclapply(
      X = seq_len(chains),
      FUN = function(X) {
        post <- mgcv::gam.mh(multinomial_model, ...)
        # post <- gam.mh(multinomial_model, t.df = 40, burn = 20000, ns = 10000, thin = 25, rw.scale = 0.025)
        post
      },
      mc.cores = cores,
      mc.set.seed = TRUE
    ))

  gam_posterior <-
    structure(
      aperm(abind::abind(samps$bs, along = 3), c(1, 3, 2)),
      rw.accept = unlist(samps$rw.accept),
      accept = unlist(samps$accept)
    )
  dimnames(gam_posterior) <- list(
    Iteration = NULL, Chain = NULL, Parameter = dimnames(gam_posterior)[[3]]
  )

  gam_posterior
}
