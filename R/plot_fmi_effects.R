#' Make a plot of the marginal effects of FMI on CoV positivity
plot_fmi_effects <- function(dat_prepped, multinomial_model, gam_posterior) {

  K = multinomial_model$family$nlp
  dat_prepped <- dat_prepped |>
    filter(sample_type == "Rectal")
  newdat <- crossing(
    dummy_rectal = 1,
    fmi_normalized = seq(-2, 2,
                    length.out = 100),
    sample_type = "Rectal",
    day = mean(dat_prepped$day),
    day_of_year = mean(dat_prepped$day_of_year),
    gender_age = factor("M-A"),
    swab = 1,
    reproductive_condition = factor("None"),
    dummy_repro = 1,
    dummy_any_rectal = 1,
    frac_subadult = mean(dat_prepped$frac_subadult, na.rm = TRUE)
  )

  post <- do.call(rbind, apply(gam_posterior, 2, identity, simplify = FALSE))
 # post <- post[sample(nrow(post), 30), ]
  pmat <-
    predict(multinomial_model, newdata = newdat, type = "lpmatrix", unconditional = TRUE)

  lpi <- attr(pmat, "lpi")
  preds <- lapply(lpi, function(i) {
    pmat[, i] %*% t(post[, i])
  }) |>
    map_dfr(function(x) {
      colnames(x) <- seq_len(ncol(x))
      bind_cols(newdat, as_tibble(x)) |>
        pivot_longer(matches("^\\d+$"), names_to = ".iteration", values_to = "linpred") |>
        mutate(.iteration = as.integer(.iteration))
    }, .id = "outcome") |>
    group_by(fmi_normalized, .iteration, outcome) |>
    mutate(prob = exp(linpred) / (1 + sum(exp(linpred)))) |>
    ungroup() |>
    mutate(vir = recode(outcome, `1` = "Novel Alpha-CoV", `2`="HKU9-related Beta-CoV", `3`="Novel Beta-CoV"))

  intervals <- preds |>
    ungroup() |>
    arrange(fmi_normalized) |>
    group_by(vir, fmi_normalized) |>
    ggdist::point_interval(prob, .point = mean, .interval = ggdist::qi, .width = c(0.95))


  pal <- colorspace::qualitative_hcl(palette = "Dark 3", n = 4)
  pal <- c(rbind(colorspace::darken(pal, 0.5), colorspace::lighten(pal, 0.3)))[c(3,5,7)]

  outcomes <- as_tibble(model.matrix(~0 + outcome, mutate(select(dat_prepped, outcome), outcome = as.factor(outcome)))[, -1])

  zz <- dat_prepped |>
    filter(sample_type == "Rectal") |>
    select(-outcome) |>
    bind_cols(outcomes) |>
    pivot_longer(cols = starts_with("outcome"), names_to="outcome", values_to="positive") |>
    mutate(outcome = substr(outcome, 8, 8)) |>
    mutate(bin = cut_width(fmi_normalized,  5)) |>
    group_by(bin, outcome) |>
    summarize(x = sum(positive), n = n(), .groups = "drop") |>
    mutate(vir = recode(outcome, `1` = "Novel Alpha-CoV", `2`="HKU9-related Beta-CoV", `3`="Novel Beta-CoV", `All` = "All CoVs"),
           bin_center = map_dbl(bin, \(x) mean(as.numeric(c(
             stringi::stri_extract_first_regex(x, "[\\d\\.]+"),
             stringi::stri_extract_last_regex(x, "[\\d\\.]+"))))))
  zz <- bind_cols(
    zz,
    select(binom::binom.exact(zz$x, zz$n), mean, lower, upper)
  )
#  breaks = 10^((-8):0)
  fig_fmi_effects <- ggplot(intervals) +
    geom_ribbon(mapping = aes(x = fmi_normalized, ymin = .lower, ymax = .upper, fill = vir), alpha = 0.4) +
    geom_line(mapping = aes(x = fmi_normalized, y = prob, col = vir)) +
    #geom_line(alpha = 0.3, mapping = aes(x = fmi_kg_m2, y = prob, col = vir, group = .iteration)) +
    geom_errorbar(data = zz, mapping = aes(x = bin_center, ymin = lower, ymax = upper), width = 1) +
    geom_point(data = zz, mapping = aes(x = bin_center, y = mean), pch = 21, fill = "grey90", size = 2) +
    scale_color_manual(values = pal, guide = guide_none()) +
    scale_fill_manual(values = pal, guide = guide_none()) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.45), oob = scales::rescale_none) +
#    scale_y_continuous(breaks = log10(breaks), labels = breaks, limits = log10(range(breaks)), oob = scales::rescale_none) +
    facet_wrap(~vir, nrow = 1, scales = "fixed") +
    labs(x = "Forearm Mass Index (kg/m2)", y = "Conditional Effect on CoV Positivity")

  if (!interactive()) rm(zz, pal, outcomes, preds, post, lpi, pmat, newdat, K)
  fig_fmi_effects
}
