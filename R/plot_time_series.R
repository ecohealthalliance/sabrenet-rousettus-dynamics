#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dat_cleaned
#' @param time_series
#' @return
#' @author 'Noam Ross'
#' @export
plot_time_series <- function(dat_prepped, time_series) {

  all_time_series <- time_series |>
    group_by(.iteration, date, day, day_of_year, sample_type) |>
    summarise(prob_corrected = sum(prob_corrected), .groups = "drop") |>
    mutate(outcome = "All", vir = "All CoVs") |>
    bind_rows(time_series) |>
    filter(!(sample_type == "Rectal" & date < min(dat_prepped[dat_prepped$sample_type == "Rectal", ]$date)))

  intervals <- all_time_series |>
    arrange(date) |>
    group_by(vir, sample_type) |>
    ggdist::curve_interval(
      prob_corrected,
      .along = date, .interval = "bd",
      .width = c(0.5, 0.95))



  # time_series2 <- time_series |>
  #   filter(.iteration %in% sample(unique(.iteration), 20))
  #
  #
  #
  # all_time_series <- time_series |>
  #   group_by(.iteration, date, day, day_of_year, sample_type) |>
  #   summarise(prob = sum(prob), .groups = "drop") |>
  #   mutate(outcome = "All", vir = "All CoVs") |>
  #   bind_rows(time_series) |>
  #   filter(!(sample_type == "Rectal" & date < min(dat_prepped[dat_prepped$sample_type == "Rectal", ]$date)))

  outcomes <- as_tibble(model.matrix(~0 + outcome, mutate(select(dat_prepped, outcome), outcome = as.factor(outcome)))[, -1])

  dat_sum <- dat_prepped |>
    select(-outcome) |>
    bind_cols(outcomes) |>
    pivot_longer(cols = starts_with("outcome"), names_to="outcome", values_to="positive") |>
    mutate(outcome = substr(outcome, 8, 8)) |>
    group_by(date, day, day_of_year, sample_type, outcome) |>
    summarise(x = sum(positive), n = n(), .groups = "drop")

  all_dat_sum <- dat_sum |>
    group_by(date, day, day_of_year, sample_type) |>
    summarize(x = sum(x), n = unique(n), .groups = "drop") |>
    mutate(outcome = "All") |>
    bind_rows(dat_sum) |>
    mutate(vir = recode(outcome, `1` = "Novel Alpha-CoV", `2`="HKU9-related Beta-CoV", `3`="Novel Beta-CoV", `All` = "All CoVs"))

  all_dat_sum <- bind_cols(
    all_dat_sum,
    select(binom::binom.exact(all_dat_sum$x, all_dat_sum$n), mean, lower, upper)
  ) |>
    mutate(across(c(mean, lower, upper), \(x) if_else(sample_type == "Fecal", correct_pooled_prevalence(x*n, n, 3), x)))

  breaks <- sort(unique(all_dat_sum$date))
  breaks <- breaks[c(10, diff(breaks)) > 2]
  pal <- colorspace::qualitative_hcl(palette = "Dark 3", n = 4)
  pal <- c(rbind(colorspace::darken(pal, 0.5), colorspace::lighten(pal, 0.3)))

  fig_time_series <- ggplot(intervals, aes(x = date)) +
    geom_vline(xintercept = as.POSIXct(lubridate::ymd(c("2018-01-01", "2019-01-01"))), lwd = 0.5, col="grey", lty=5) +
    geom_ribbon(data = filter(intervals, .width == 0.95), mapping = aes(ymin = .lower, ymax = .upper, fill = paste(vir, sample_type)), alpha = 0.4) +
  #  geom_ribbon(data = filter(intervals, .width == 0.5), mapping = aes(ymin = .lower, ymax = .upper, group = paste(vir, sample_type), col = paste(vir, sample_type)), fill = NA) +
    geom_line(data = filter(intervals, .width == 0.5), mapping = aes(y = prob_corrected, group = paste(vir, sample_type), linetype = sample_type), col = "black", linewidth = .75) +
    geom_errorbar(data = all_dat_sum, mapping = aes(ymin = lower, ymax = upper), col = "grey10") +
    geom_point(data = all_dat_sum, mapping = aes(y = mean, pch = sample_type), fill = "grey10", alpha = 1) +
    scale_y_continuous(labels = scales::percent, name = "% CoV Positive", breaks = \(x) seq(0, x[2], by = 0.2)) +
    scale_x_datetime(date_labels = "%d-%b", breaks = breaks, name = "Sampling Date") +
    scale_color_manual(values = pal, guide = "none") +
    scale_fill_manual(values = pal, guide = "none") +
    scale_linetype_manual(values = c(3, 1), name = "Modeled Values") +
    scale_shape_manual(values = c(21, 1), name = "Measured Positivity") +
    ggforce::facet_col(~vir, scales = "free_y", space = "free") +
    theme(axis.text.x = element_text(angle = 60, vjust =0.7, hjust = 0.5, size = rel(0.75)), axis.ticks = element_blank(),
          panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), axis.title.x = element_text(),
          legend.position = "bottom")

  if (!interactive()) rm(all_dat_sum, dat_sum, outcomes, all_time_series, time_series)
  fig_time_series

}
