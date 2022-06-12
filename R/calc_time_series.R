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
  newdat <- crossing(
    date = seq(min(dat_cleaned$date_collected),
               max(dat_cleaned$date_collected),
               by = "day"),
    sample_type = as.factor(c("Fecal", "Rectal"))) |>
    mutate(day = as.numeric(date - min(date), "days"),
           day_of_year = lubridate::yday(date),
           dummy_rectal = as.integer(sample_type == "Rectal"))

   newdat <- left_join(
     newdat,
     dat_prepped |>
       group_by(date, sample_type) |>
       summarize(fmi_kg_m2 = mean(fmi_kg_m2, na.rm = TRUE), .groups = "drop"),
     by = c("date", "sample_type")
   ) |>
     arrange(sample_type, date)

   interp <- newdat |>
     filter(sample_type == "Rectal", !is.na(fmi_kg_m2))

   newdat$fmi_kg_m2[newdat$sample_type == "Rectal"] <-
     approx(interp$day, interp$fmi_kg_m2, newdat[newdat$sample_type == "Rectal", ]$day, method = "linear", rule = 2)$y

   newdat <- newdat |>
     mutate(fmi_kg_m2 = coalesce(fmi_kg_m2, 0))

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
    ungroup() |>
    mutate(vir = recode(outcome, `1` = "Novel Alpha-Cov", `2`="HKU9-related Beta-CoV", `3`="Novel Beta-CoV"))


  time_series
}
