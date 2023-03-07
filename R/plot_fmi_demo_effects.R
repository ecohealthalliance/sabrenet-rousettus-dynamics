sums <- function(x) {
  rg <- signif(range(x), 3)
  glue::glue("{signif(mean(x), 3)}, {signif(median(x), 3)}, ({rg[1]}-{rg[2]})")
}

tabulate_fmi_demo <- function(dat_prepped) {
  tab_data <- dat_prepped |>
    filter(sample_type == "Rectal")

  tab_demo_data <- tab_data |>
    summarize(n = n(), pos = sum(cov_detected), sums_fmi = sums(fmi_kg_m2),
              sums_mass = sums(mass_g), sums_fa = sums(fa_mm),
              .by = demo_group
              ) |>
    rename(group = demo_group)

  tab_age_data <- tab_data |>
    summarize(n = n(), pos = sum(cov_detected),
              sums_fmi = sums(fmi_kg_m2), sums_mass = sums(mass_g), sums_fa = sums(fa_mm),
              .by = age) |>
    rename(group = age)

  tab_all_data <-  tab_data |>
    summarize(n = n(), pos = sum(cov_detected),
              sums_fmi = sums(fmi_kg_m2), sums_mass = sums(mass_g), sums_fa = sums(fa_mm)) |>
    mutate(group = "All")

  tab_sum <- bind_rows(tab_all_data, tab_age_data, tab_demo_data)

  renames = c("All" = "All Sampled Bats",
              "A" = "Adults",
              "SA" = "Subadults",
              "F SA Not pregnant" = "Subadults, Females",
              "M SA Not scrotal" = "Subadults, Males",
              "F A Not pregnant" = "Adults, Not Pregrant or Lactating Females",
              "F A Lactating" = "Adults, Lactating Females",
              "F A Pregnant" = "Adults, Pregnant Females",
              "M A Not scrotal" = "Adults, Nonscrotal Males",
              "M A Scrotal" = "Adults, Scrotal Males")

  table_fmi_demo <- bind_cols(
    tab_sum,
    select(binom::binom.exact(tab_sum$pos, tab_sum$n), -x, -n)
  ) |>
    mutate(sums_cov = glue::glue("{scales::percent(mean, 0.1)} ({scales::percent(lower, 0.1)}-{scales::percent(upper, 0.1)})")) |>
    select(group, sums_fmi, sums_mass, sums_fa, sums_cov) |>
    mutate(group = renames[group]) |>
    mutate(group = fct_relevel(group, renames)) |>
    arrange(group) |>
    rename(Subgroup = group, `FMI (kg/m^2)` = sums_fmi, `Mass (g)`=sums_mass, `Forearm (mm)` = sums_fa, `CoV Prevalance`=sums_cov)

  table_fmi_demo
}

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dat_prepped
#' @param multinomial_model
#' @param gam_posterior
#' @return
#' @author Noam Ross
#' @export
plot_fmi_demo_effects <- function(dat_prepped) {
  plot_data <- dat_prepped |>
    filter(sample_type == "Rectal") |>
    mutate(id = seq_len(n())) |>
    group_by(demo_group) |>
    mutate(positivity = sum(cov_detected) / n())

  point_plot_data <- plot_data |>
    group_by(demo_group) |>
    summarize(mean_fmi = mean(fmi_kg_m2), sd_fmi = sd(fmi_kg_m2), n = n(), pos = sum(cov_detected))
  point_plot_data <- bind_cols(
    point_plot_data,
    select(binom::binom.exact(point_plot_data$pos, point_plot_data$n), -x, -n)
  )
  fig_fmi_demo_effects <- ggplot(
    point_plot_data,
    aes(fill = demo_group, label = demo_group, x = mean_fmi, y = mean, ymin = lower, ymax = upper, xmin = mean_fmi - 2 * sd_fmi, xmax = mean_fmi + 2 * sd_fmi, color = demo_group)
  ) +
    geom_smooth(mapping = aes(group = 1), method = "lm", col = "grey10", lty = 2) +
    geom_errorbar(lwd = 1, width = 0.5) +
    geom_errorbarh(lwd = 1) +
    geom_point(pch = 21, size = 4, color = "black") +
    ggrepel::geom_label_repel(color = "black") +
    theme(legend.position = "none") +
    scale_x_continuous(limits = c(8, 21.5), oob = scales::rescale_none) +
    scale_y_continuous(limits = c(0, 0.25), oob = scales::rescale_none) +
    labs(x = "Group FMI", y = "Group Positivity")

  fig_fmi_demo_effects
}

plot_fmi_demo_timeseries <- function(dat_prepped) {
  plot_data <- dat_prepped |>
    filter(sample_type == "Rectal") |>
    group_by(demo_group) |>
    group_by(date, demo_group) |>
    summarize(mean_fmi = mean(fmi_kg_m2), sd_fmi = sd(fmi_kg_m2), n = n())

  fig_fmi_demo_timeseries <- ggplot(
    plot_data,
    aes(x = date, y = mean_fmi, ymin = mean_fmi - 2 * sd_fmi, ymax = mean_fmi + 2 * sd_fmi, color = demo_group, fill = demo_group)
  ) +
    facet_wrap(~demo_group, ncol = 2) +
    geom_ribbon(alpha = 0.4) +
    geom_line() +
    geom_point() +
    geom_text(mapping = aes(label = n), size = 2, nudge_y = 1.5, col = "black")

  fig_fmi_demo_timeseries
}

plot_positivity_demo_timeseries <- function(dat_prepped) { # nolint
  plot_data <- dat_prepped |>
    filter(sample_type == "Rectal") |>
    group_by(date, demo_group) |>
    summarize(pos = sum(cov_detected), count = n()) |>
    ungroup()

  plot_data <- bind_cols(
    plot_data,
    select(binom::binom.exact(plot_data$pos, plot_data$count), -x, -n, frac = mean)
  )

  fig_pos_demo_timeseries <- ggplot(plot_data, aes(x = date, y = frac, ymin = lower, ymax = upper, color = demo_group, fill = demo_group)) +
    facet_wrap(~demo_group, ncol = 2) +
    geom_ribbon(alpha = 0.4) +
    geom_line() +
    geom_point() +
    geom_text(mapping = aes(label = count), size = 2, nudge_y = 0.1, col = "black")

  fig_pos_demo_timeseries
}
