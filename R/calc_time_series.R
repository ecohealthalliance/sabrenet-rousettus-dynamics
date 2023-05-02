#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param multinomial_model
#' @param gam_posterior
#' @return
#' @author 'Noam Ross'
#' @export
calc_time_series <- function(dat_cleaned, dat_prepped, multinomial_model, gam_posterior) {
  K <- multinomial_model$family$nlp #nolint

  interps <- dat_prepped |>
    mutate(
      day = as.numeric(date - min(date), "days"),
      week = lubridate::floor_date(date, "week")
    ) |>
    group_by(week) |>
    mutate(day = mean(unique(day))) |>
    group_by(day) |>
    summarize(
      n_fecal_samps = sum(sample_type == "Fecal")
    )

  n_fecal_samps_fn <- splinefun(x = interps$day, y = interps$n_fecal_samps)

  newdat <-
    crossing(
      date = seq(min(dat_cleaned$date_collected),
                 max(dat_cleaned$date_collected),
                 by = "week"
      ),
      sample_type = as.factor(c("Fecal", "Rectal")),
      gender_age = factor("NA"),
      dummy_repro = ordered(0),
      reproductive_condition = factor("None")
    ) |>
    mutate(
      day = as.numeric(date - min(date), "days"),
      day_of_year = lubridate::yday(date),
      fmi_normalized = 0,
      n_fecal_samps = n_fecal_samps_fn(day),
      dummy_rectal = ordered(as.integer(sample_type == "Rectal"))
    ) |>
    arrange(sample_type, date)

  post <- do.call(rbind, apply(gam_posterior, 2, identity, simplify = FALSE))
  keep_cols <- which(!stringi::stri_detect_fixed(colnames(post), "gender_age"))

  post <- post[sample.int(nrow(post), pmin(100, nrow(post))), ]
  pmat <-
    predict(multinomial_model, newdata = newdat, type = "lpmatrix", unconditional = TRUE)
  lpi <- attr(pmat, "lpi")
  time_series <- lapply(lpi, function(i) {
    j <- i[i %in% keep_cols]
    pmat[, j] %*% t(post[, j])
  }) |>
    map_dfr(function(x) {
      colnames(x) <- seq_len(ncol(x))
      bind_cols(newdat, as_tibble(x)) |>
        pivot_longer(matches("^\\d+$"), names_to = ".iteration", values_to = "linpred") |>
        mutate(.iteration = as.integer(.iteration))
    }, .id = "outcome") |>
    group_by(date, sample_type, .iteration) |>
    mutate(prob = exp(linpred) / (1 + sum(exp(linpred)))) |>
    mutate(prob_corrected = if_else(sample_type == "Fecal", correct_pooled_prevalence(n_fecal_samps * prob, n_fecal_samps, 3), prob)) |>
    ungroup() |>
    mutate(vir = recode(outcome, `1` = "Novel Alpha-CoV", `2` = "HKU9-related Beta-CoV", `3` = "Novel Beta-CoV"))


  time_series
}

calc_peak_dates <- function(dat_cleaned, dat_prepped, multinomial_model, gam_posterior) {
  K <- multinomial_model$family$nlp #nolint
  newdat <-
    crossing(
      day = mean(dat_prepped$day),
      sample_type = as.factor(c("Fecal", "Rectal")),
      day_of_year = 1:365
    ) |>
    mutate(
      gender_age = factor("NA"),
      dummy_repro = ordered(0),
      reproductive_condition = factor("None"),
      fmi_normalized = 0,
      dummy_rectal = ordered(as.integer(sample_type == "Rectal"))
    ) |>
    arrange(sample_type, day_of_year)

  post <- do.call(rbind, apply(gam_posterior, 2, identity, simplify = FALSE))
  pmat <-
    predict(multinomial_model, newdata = newdat, type = "lpmatrix", unconditional = TRUE)
  lpi <- attr(pmat, "lpi")
  time_series <- lapply(lpi, function(i) {
    pmat[, i] %*% t(post[, i])
  }) |>
    map_dfr(function(x) {
      colnames(x) <- seq_len(ncol(x))
      bind_cols(newdat, as_tibble(x)) |>
        pivot_longer(matches("^\\d+$"), names_to = ".iteration", values_to = "linpred") |>
        mutate(.iteration = as.integer(.iteration))
    }, .id = "outcome")

  peak_dates <- time_series |>
    group_by(outcome, sample_type, .iteration) |>
    filter(linpred == max(linpred)) |>
    group_by(day_of_year, sample_type, .iteration) |>
    mutate(prob = exp(linpred) / (1 + sum(exp(linpred))),
           vir = recode(outcome, `1` = "Novel Alpha-CoV", `2` = "HKU9-related Beta-CoV", `3` = "Novel Beta-CoV")) |>
    select(vir, day_of_year, sample_type, linpred, prob, .iteration) |>
    mutate(date = as.Date("2019-12-30") + days(day_of_year))


}

plot_peak_dates <- function(peak_dates) {

  peak_date_sum <- peak_dates |>
    group_by(vir, sample_type) |>
    ggdist::mean_qi(day_of_year) |>
    mutate(across(c("day_of_year", ".lower", ".upper"), \(x) as.Date("2019-12-30") + days(round(x)), .names = "{.col}_date")) |>
    mutate(label = glue::glue("{sample_type} peak: {strftime(day_of_year_date, '%b %d')} (95% CI {strftime(.lower_date, '%b %d')}-{strftime(.upper_date, '%b %d')})"))

  labs <- peak_date_sum |>
    group_by(vir) |>
    summarize(label = paste(label, collapse = "\n"))

  ggplot() +
    geom_density(data = peak_dates, mapping = aes(x = date, fill = vir, linetype = sample_type), alpha = 0.5, key_glyph = draw_key_path) +
    geom_text(data = labs, mapping = aes(label = label, x = as.Date("2020-12-15"), y = 0.04), hjust=1, size=3) +
    facet_wrap(~vir, ncol=1) +
    scale_fill_manual(values = colorspace::qualitative_hcl(3), guide = "none") +
    scale_linetype_discrete(name = "") +
    scale_x_date(date_labels = "%b", name = "") +
    #scale_linetype_manual(values = c(3, 1), name = "") +
    #guides(linetype = guide_legend(override.aes = list(shape = 3))) +
    theme(legend.position = c(0.13,0.13), legend.background = element_blank()) + labs(y = "Posterior Density")
}
