get_day_age_repro_effects <- function(multinomial_model, gam_posterior, dat_prepped) {

  newdat <-
    crossing(
      gender_age = factor(c("F-A", "F-SA", "M-A", "M-SA")),
      reproductive_condition = factor(c("None", "Lactating", "Pregnant", "Scrotal")),
      day = round(seq(min(dat_prepped$day), max(dat_prepped$day), by = 1))
    ) |>
    mutate(sample_type = factor("Rectal"), day_of_year=yday(min(dat_prepped$date) + days(day)),
           fmi_normalized = 0,
           #gender = factor("NA"), gender_age = factor("NA"),
           #reproductive_condition = factor("None"),
           dummy_rectal = 1, dummy_repro=1)

  pmat <- predict(multinomial_model, type = "lpmatrix", newdata = newdat)
  lpi <- attr(pmat, "lpi")
  post <- do.call(rbind, apply(gam_posterior, 2, identity, simplify = FALSE))
  day_age_repro_effects0 <- lapply(lpi, function(i) {
    pmat[, i] %*% t(post[, i])
  }) |>
    map_dfr(function(x) {
      colnames(x) <- seq_len(ncol(x))
      bind_cols(newdat, as_tibble(x)) |>
        pivot_longer(matches("^\\d+$"), names_to = ".iteration", values_to = "linpred") |>
        mutate(.iteration = as.integer(.iteration))
    }, .id = "outcome") |>
    group_by(day, gender_age, reproductive_condition, fmi_normalized, .iteration) |>
    mutate(prob = exp(linpred) / (1 + sum(exp(linpred)))) |>
    ungroup() |>
    mutate(vir = recode(outcome, `1` = "Novel Alpha-CoV", `2` = "HKU9-related Beta-CoV", `3` = "Novel Beta-CoV")) |>
    select(day, gender_age, reproductive_condition, fmi_normalized, vir, linpred, prob, .iteration)

  day_age_repro_effects <- day_age_repro_effects0 |>
    group_by(day, gender_age, reproductive_condition, fmi_normalized, vir) |>
    ggdist::mean_qi(linpred, prob) |>
    mutate(date = min(dat_prepped$date) + days(day))

  day_age_repro_effects
}

plot_day_age_repro_effects <- function(day_age_repro_effects, dat_prepped) {

  obs <- dat_prepped |>
    filter(sample_type == "Rectal") |>
    summarize(pos = sum(vir == "HKU9-related Beta-CoV", na.rm = TRUE), n = n(),
              .by = c(reproductive_condition, age, date, day)) |>
    filter(n > 0) |>
    mutate(`Reproductive Condition` = fct_recode(reproductive_condition, `Baseline` = "None", `Scrotal Males`= "Scrotal") |>
             fct_relevel("Baseline", after = 0)) |>
    mutate(`Age Class` = fct_recode(age, `Adults (Baseline, >=89mm)` = "A", `Subadults (<89mm)` = "SA"))

  day_age_repro_effects2 <- day_age_repro_effects |>
    ungroup() |>
    filter(vir == "HKU9-related Beta-CoV", between(date, min(obs$date), max(obs$date))) |>
    mutate(`Age Class` = factor(if_else(gender_age %in% c("F-A", "M-A"), "Adults (Baseline, >=89mm)", "Subadults (<89mm)"))) |>
    mutate(`Reproductive Condition` = fct_recode(reproductive_condition, `Baseline` = "None", `Scrotal Males`= "Scrotal") |>
             fct_relevel("Baseline", after = 0))

  obs_age <- obs |>
    summarize(pos = sum(pos), n = sum(n), .by = c(`Age Class`, date)) |>
    mutate(dodge = if_else(`Age Class` == "Adults (Baseline, >=89mm)", 150000, -150000))
  obs_age <- bind_cols(obs_age, binom::binom.exact(obs_age$pos, obs_age$n) |> select(mean, lower, upper))

  modvals_age <- day_age_repro_effects2 |>
    filter(`Reproductive Condition` == "Baseline") #+
    #distinct(day, date, `Age Class`, .keep_all = TRUE)

  modvals_repro <- day_age_repro_effects2 |>
    filter(`Age Class` == "Adults (Baseline, >=89mm)") #+
    #distinct(date, `Reproductive Condition`, .keep_all = TRUE)

  colors <- colorspace::qualitative_hcl(5)

  age_repro_time_plot <- ggplot() +
    geom_ribbon(data = modvals_age, mapping = aes(x=date, ymin = prob.lower, ymax = prob.upper, fill = `Age Class`, col = `Age Class`, linetype = `Age Class`), alpha = 0.1, size = 0.5) +
    geom_line(data = modvals_age, mapping = aes(x=date, y = prob, col = `Age Class`, linetype = `Age Class`, size = `Age Class`), size = 2) +
    scale_linetype_manual(values = c(1, 2), guide = guide_none()) +
    scale_color_discrete(type = colors[c(1,3)], guide = guide_legend(order = 2), name = "Age Class\n(Modeled)") +
    scale_fill_discrete(type = colors[c(1,3)], guide = guide_legend(order = 2), name = "Age Class\n(Modeled)") +
    ggnewscale::new_scale_color() +
    scale_color_discrete(type = colors[c(1,3)], guide = guide_legend(order = 1), name = "Age Class\n(Observed)", labels = c("Adults (All, >=89mm)", "Subadults (<89mm)")) +
    geom_errorbar(data = obs_age, mapping = aes(x = date + dodge, ymin = lower, ymax = upper, col = `Age Class`), alpha  = 1, key_glyph="blank") +
    geom_point(data = obs_age, mapping = aes(x = date + dodge, mean, col = `Age Class`), size =2, key_glyph = "point") +
    scale_x_datetime(date_labels = "%b '%y", date_breaks = "3 months", expand = expansion(0.01)) +
    scale_y_continuous(name = "Prevalance", labels = scales::percent) +
    ggnewscale::new_scale_color() +
    geom_line(data = modvals_repro, mapping = aes(x = date, y = prob, col = `Reproductive Condition`, alpha = `Reproductive Condition`)) +
    scale_color_discrete(type = colors[c(1,2,4,5)], guide = guide_legend(order = 3), name = "Reproductive Condition\n(Modeled)") +
    scale_alpha_manual(values = c(0, 1, 1, 1), guide = guide_none()) +
    theme(axis.title.x = element_blank())
  age_repro_time_plot
}
