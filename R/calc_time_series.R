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

  K = multinomial_model$family$nlp

  interps <- dat_prepped |>
    mutate(day = as.numeric(date - min(date), "days"),
           week = lubridate::floor_date(date, "week")) |>
    group_by(week) |>
    mutate(day = mean(unique(day))) |>
    ungroup() |>
    group_by(day) |>
    summarize(frac_subadult = unique(frac_subadult),
              fmi_kg_m2 = mean(fmi_kg_m2[sample_type == "Rectal"],),
              n_fecal_samps = sum(sample_type == "Fecal"))
  frac_subadult_fn <- splinefun(x = interps$day, y = interps$frac_subadult)
  fmi_kg_m2_fn <- splinefun(x = interps$day[is.finite(interps$fmi_kg_m2)], y = interps$fmi_kg_m2[is.finite(interps$fmi_kg_m2)])
  n_fecal_samps_fn <- splinefun(x = interps$day, y = interps$n_fecal_samps)

  newdat <- crossing(
    date = seq(min(dat_cleaned$date_collected),
               max(dat_cleaned$date_collected),
               by = "day"),
    sample_type = as.factor(c("Fecal", "Rectal")),
    gender_age = factor("NA"),
    dummy_repro = ordered(0),
    # fmi_kg_m2 = multinomial_model$model$fmi_kg_m2 |> table() |> sort() |> rev() |> names() |> head(1) |> as.numeric(),
    # frac_subadult = multinomial_model$model$frac_subadult |> table() |> sort() |> rev() |> names() |> head(1) |> as.numeric(),
    reproductive_condition = factor("None"),
    dummy_any_rectal = 1)|>
    mutate(day = as.numeric(date - min(date), "days"),
           day_of_year = lubridate::yday(date),
           frac_subadult = frac_subadult_fn(day),
           fmi_kg_m2 = fmi_kg_m2_fn(day),
           n_fecal_samps = n_fecal_samps_fn(day),
           dummy_rectal = ordered(as.integer(sample_type == "Rectal"))
    ) |>
    arrange(sample_type, date)

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
    }, .id = "outcome") |>
    group_by(date, sample_type, .iteration,) |>
    mutate(prob = exp(linpred) / (1 + sum(exp(linpred)))) |>
    mutate(prob_corrected = if_else(sample_type == "Fecal", correct_pooled_prevalence(n_fecal_samps*prob, n_fecal_samps, 3), prob)) |>
    ungroup() |>
    mutate(vir = recode(outcome, `1` = "Novel Alpha-Cov", `2`="HKU9-related Beta-CoV", `3`="Novel Beta-CoV"))


  time_series
}
