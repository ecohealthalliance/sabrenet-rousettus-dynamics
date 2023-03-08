calc_raw_prev <- function(dat_prepped) {
  raw_prev_0 <- dat_prepped |>
    group_by(sample_type, outcome) |>
    summarize(positives = n()) |>
    group_by(sample_type) |>
    mutate(n = sum(positives)) |>
    mutate(positives = if_else(outcome == 0, sum(positives) - positives, positives)) |>
    mutate(outcome = recode(outcome, `1` = "Novel Alpha-CoV", `2` = "HKU9-related Beta-CoV", `3` = "Novel Beta-CoV", `0` = "All CoVs")) |>
    rename(virus = outcome)

  raw_prev <- raw_prev_0 |>
    bind_cols(binom::binom.exact(x = raw_prev_0$positives, n = raw_prev_0$n) |> select(mean, lower, upper)) |>
    mutate(across(c(mean, lower, upper), ~ if_else(sample_type == "Fecal", correct_pooled_prevalence(. * n, n, 3), NA_real_), .names = "{.col}_corrected")) |>
    mutate(calcuation_type = "raw") |>
    mutate(across(matches("^(mean|lower|upper)\\.*"), scales::percent)) |>
    select(sample_type, calcuation_type, everything())

  raw_prev
}

softmax <- function(x) {
  exp(x) / (1 + rowSums(exp(x)))
}

calc_model_prev <- function(dat_prepped, multinomial_model, gam_posterior) {
  post <- do.call(rbind, apply(gam_posterior, 2, identity, simplify = FALSE))
  fecal_intercepts <- softmax(post[, c("(Intercept)", "(Intercept).1", "(Intercept).2")])
  fecal_intercepts <- cbind(fecal_intercepts, rowSums(fecal_intercepts))
  rectal_intercepts <- softmax(post[, c("(Intercept)", "(Intercept).1", "(Intercept).2")] +
    post[, stringi::stri_subset_regex(colnames(post), "\\(sample_type\\):dummy_rectal1\\.2")])
  rectal_intercepts <- cbind(rectal_intercepts, rowSums(rectal_intercepts))

  colnames(fecal_intercepts) <- c("HKU9-related Beta-CoV", "Novel Alpha-CoV", "Novel Beta-CoV", "All CoVs")
  colnames(rectal_intercepts) <- c("HKU9-related Beta-CoV", "Novel Alpha-CoV", "Novel Beta-CoV", "All CoVs")
  model_fecal_prev_0 <- fecal_intercepts |>
    as_tibble() |>
    pivot_longer(everything(), names_to = "virus", values_to = "mean") |>
    mutate(sample_type = "Fecal")
  model_rectal_prev_0 <- rectal_intercepts |>
    as_tibble() |>
    pivot_longer(everything(), names_to = "virus", values_to = "mean") |>
    mutate(sample_type = "Rectal")

  model_prev_0 <- bind_rows(model_fecal_prev_0, model_rectal_prev_0) |>
    group_by(virus, sample_type) |>
    ggdist::point_interval(mean, .point = mean, .interval = ggdist::hdci) |>
    rename(lower = .lower, upper = .upper) |>
    select(-.width, -.point, -.interval)
  model_prev <- model_prev_0 |>
    mutate(calculation_type = "modeled") |>
    mutate(across(c(mean, lower, upper), ~ if_else(sample_type == "Fecal", correct_pooled_prevalence(. * 30, 30, 3), NA_real_), .names = "{.col}_corrected")) |>
    mutate(across(matches("^(mean|lower|upper)\\.*"), ~ scales::percent(., accuracy = 0.1))) |>
    arrange(sample_type, virus)

  model_prev
}
