calc_raw_fecal_prev <- function(dat_prepped) {

  fec_data <- dat_prepped |>
    filter(sample_type == "Fecal")

  raw_prev <- binom::binom.confint(
    x = sum(fec_data$cov_detected),
    n = nrow(fec_data),
    methods = "exact")

  corrected_prev <- lapply(raw_prev[,c("mean", "lower", "upper")], function(x) {
    correct_pooled_prevalence(
      round(x*raw_prev$n),
      raw_prev$n,
      3
    )})
  names(corrected_prev) <- paste0(names(corrected_prev), "_corrected")
  bind_cols(raw_prev, as.data.frame(corrected_prev))
}

softmax <- function(x) {
  exp(x) / (1 + rowSums(exp(x)))
}

calc_model_fecal_prev <- function(dat_prepped, multinomial_model, gam_posterior) {
  post <- do.call(rbind, apply(gam_posterior, 2, identity, simplify = FALSE))
  all_cov_fecal_intercept <- rowSums(softmax(post[, c("(Intercept)", "(Intercept).1", "(Intercept).2")]))
  all_cov_fecal_est <- ggdist::point_interval(all_cov_fecal_intercept,  .point = mean, .interval = ggdist::hdi)
  all_cov_rectal_intercept <- rowSums(softmax(post[, c("(Intercept)", "(Intercept).1", "(Intercept).2")] + post[, stringi::stri_subset_regex(colnames(post), "\\(sample_type\\):dummy_rectal1\\.2")]))
  all_cov_rectal_est <- ggdist::point_interval(all_cov_rectal_intercept, .point = mean, .interval = ggdist::hdi)

  corrected_prev <- lapply(all_cov_fecal_est[,c("y", "ymin", "ymax")], function(x) {
    correct_pooled_prevalence(
      round(x*sum(dat_prepped$sample_type == "Fecal")),
      sum(dat_prepped$sample_type == "Fecal"),
      3
    )})
  names(corrected_prev) <- paste0(names(corrected_prev), "_corrected")
  bind_cols(all_cov_rectal_est, as.data.frame(corrected_prev))

  pmat <-
    predict(multinomial_model, type = "lpmatrix", unconditional = TRUE)
  lpi <- attr(pmat, "lpi")
  time_series <- lapply(lpi, function(i) {
    pmat[, i] %*% t(post[, i])
  })

      map_dfr(function(x) {
      colnames(x) <- seq_len(ncol(x))
      bind_cols(newdat, as_tibble(x)) |>
        pivot_longer(matches("^\\d+$"), names_to = ".iteration", values_to = "linpred") |>
        mutate(.iteration = as.integer(.iteration))
    }, .id = "outcome")
}

