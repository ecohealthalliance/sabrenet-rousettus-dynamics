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
