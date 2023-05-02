get_repro_effects <- function(multinomial_model, gam_posterior, dat_prepped) {

  newdat <- dat_prepped |>
    filter(sample_type == "Rectal") |>
    mutate(day = mean(day), day_of_year=mean(day_of_year), fmi_normalized = 0) |>
    distinct(reproductive_condition, .keep_all = TRUE)

  pmat <- predict(multinomial_model, type = "lpmatrix", newdata = newdat)
  lpi <- attr(pmat, "lpi")
  post <- do.call(rbind, apply(gam_posterior, 2, identity, simplify = FALSE))
  repro_effects0 <- lapply(lpi, function(i) {
    pmat[, i] %*% t(post[, i])
  }) |>
    map_dfr(function(x) {
      colnames(x) <- seq_len(ncol(x))
      bind_cols(newdat, as_tibble(x)) |>
        pivot_longer(matches("^\\d+$"), names_to = ".iteration", values_to = "linpred") |>
        mutate(.iteration = as.integer(.iteration))
    }, .id = "outcome") |>
    group_by(reproductive_condition, .iteration) |>
    mutate(prob = exp(linpred) / (1 + sum(exp(linpred)))) |>
    ungroup() |>
    mutate(vir = recode(outcome, `1` = "Novel Alpha-CoV", `2` = "HKU9-related Beta-CoV", `3` = "Novel Beta-CoV")) |>
    select(reproductive_condition, vir, linpred, prob, .iteration)

  repro_effects_ave <- repro_effects0 |>
    group_by(reproductive_condition, vir) |>
    ggdist::mean_qi(linpred, prob)

  repro_effects_ranks <- repro_effects0 |>
    group_by(vir, .iteration) |>
    mutate(rank = rank(prob)) |>
    group_by(reproductive_condition, vir) |>
    summarize(pct_first = sum(rank == 4)/n(), pct_last = sum(rank==1)/n()) |>
    arrange(vir, reproductive_condition)

  repro_effects <- left_join(
    repro_effects_ave,
    repro_effects_ranks,
    by = c("reproductive_condition", "vir")
  ) |>
    arrange(vir, reproductive_condition)

  repro_effects

}

plot_repro_effects <- function(repro_effects) {
  pal <- colorspace::qualitative_hcl(palette = "Dark 3", n = 4)
  pal2 <- colorspace::darken(pal, 0.5)

  repro_effects2 <- repro_effects |>
    mutate(labels = glue::glue("{reproductive_condition}\nHighest: {scales::percent(pct_first)}\nLowest: {scales::percent(pct_last)}"),
           labels = factor(labels, labels)) |>
    filter(vir == "HKU9-related Beta-CoV")

  ggplot(repro_effects2, aes(x = labels, y = prob, ymin = prob.lower, ymax = prob.upper, col = vir, fill = vir)) +
    geom_col() +
    geom_errorbar(width = 0.2) +
    scale_color_manual(values = pal2, guide = guide_none()) +
    scale_fill_manual(values = pal, guide = guide_none()) +
    scale_y_continuous(labels = scales::percent, limits = c(-0.005,0.15), expand = c(0,0)) +
    facet_wrap(~vir, ncol = 3, drop = TRUE, scales = "free") +
    labs(x = "Reproductive Condition", y = "Conditional Prevalence")
}
