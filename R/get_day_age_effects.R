get_day_age_effects <- function(multinomial_model, gam_posterior, dat_prepped) {

  newdat <-
    crossing(
      age = factor(c("A", "SA")),
      day = round(seq(min(dat_prepped$day), max(dat_prepped$day), length.out = 100))
    ) |>
    mutate(sample_type = "Rectal", day_of_year=yday(min(dat_prepped$date) + days(day)),
           fmi_normalized = 0,
           gender = factor("NA"), gender_age = factor("NA"),
           reproductive_condition = factor("None"), dummy_rectal = 1, dummy_repro=0)

  pmat <- predict(multinomial_model, type = "lpmatrix", newdata = newdat)
  lpi <- attr(pmat, "lpi")
  post <- do.call(rbind, apply(gam_posterior, 2, identity, simplify = FALSE))
  day_age_effects0 <- lapply(lpi, function(i) {
    pmat[, i] %*% t(post[, i])
  }) |>
    map_dfr(function(x) {
      colnames(x) <- seq_len(ncol(x))
      bind_cols(newdat, as_tibble(x)) |>
        pivot_longer(matches("^\\d+$"), names_to = ".iteration", values_to = "linpred") |>
        mutate(.iteration = as.integer(.iteration))
    }, .id = "outcome") |>
    group_by(day, age, .iteration) |>
    mutate(prob = exp(linpred) / (1 + sum(exp(linpred)))) |>
    ungroup() |>
    mutate(vir = recode(outcome, `1` = "Novel Alpha-CoV", `2` = "HKU9-related Beta-CoV", `3` = "Novel Beta-CoV")) |>
    select(day, age, vir, linpred, prob, .iteration)

  day_age_effects <- day_age_effects0 |>
    group_by(day, age, vir) |>
    ggdist::mean_qi(linpred, prob) |>
    mutate(date = min(dat_prepped$date) + days(day))

  day_age_effects
}

plot_day_age_effects <- function(day_age_effects) {

  day_age_effects2 <- day_age_effects |>
    mutate(`Age Class` = fct_recode(age, Adults = "A", Subadults = "SA")) |>
    filter(vir == "HKU9-related Beta-CoV")

  ggplot(day_age_effects2, aes(x = date, y = prob, ymin = prob.lower, ymax = prob.upper,
                               col = `Age Class`, fill = `Age Class`)) +
    geom_ribbon(alpha = 0.5, linetype = 2) +
    geom_line(linewidth = 1, linetype = 1) +
    scale_color_manual(values = c("darkred", "darkgreen")) +
    scale_fill_manual(values = c("#FFFFFF00", "grey60")) +
    scale_y_continuous(labels = scales::percent, limits = c(-0,1), expand = c(0,0)) +
    scale_x_datetime(expand = c(0,0)) +
    facet_wrap(~vir, ncol = 3, drop = TRUE, scales = "free") +
    labs(x = "Date", y = "Conditional Prevalence") +
    theme(legend.position = c(0.75, 0.75), legend.background = element_blank())
}
