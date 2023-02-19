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

  time_series <- time_series |>
    filter(.iteration %in% sample(unique(.iteration), 20))

  all_time_series <- time_series |>
    group_by(.iteration, date, day, day_of_year, sample_type) |>
    summarise(prob = sum(prob), .groups = "drop") |>
    mutate(outcome = "All", vir = "All CoVs") |>
    bind_rows(time_series) |>
    filter(!(sample_type == "Rectal" & date < min(dat_prepped[dat_prepped$sample_type == "Rectal", ]$date)))

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
    mutate(vir = recode(outcome, `1` = "Novel Alpha-Cov", `2`="HKU9-related Beta-CoV", `3`="Novel Beta-CoV", `All` = "All CoVs"))


  all_dat_sum <- bind_cols(
    all_dat_sum,
    select(binom::binom.exact(all_dat_sum$x, all_dat_sum$n), mean, lower, upper)
  )

  breaks <- sort(unique(all_dat_sum$date))
  breaks <- breaks[c(10, diff(breaks)) > 2]
  pal <- colorspace::qualitative_hcl(palette = "Dark 3", n = 4)
  pal <- c(rbind(colorspace::darken(pal, 0.5), colorspace::lighten(pal, 0.3)))
  fig_time_series <- ggplot(all_time_series, aes(x = date)) +
    geom_vline(xintercept = as.POSIXct(lubridate::ymd(c("2018-01-01", "2019-01-01"))), lwd = 0.5, col="grey", lty=5) +
    geom_line(alpha = 0.4, mapping = aes(y = prob, col = paste(vir, sample_type), lty = sample_type,
              group = paste(.iteration, outcome, sample_type))) +
    geom_errorbar(data = all_dat_sum, mapping = aes(ymin = lower, ymax = upper)) +
    geom_point(data = all_dat_sum, mapping = aes(y = mean, fill = sample_type), pch = 21, alpha = 1) +
    ggforce::facet_col(~vir, scales = "free_y", space = "free") +
    scale_y_continuous(labels = scales::percent, name = "% CoV Positive", breaks = \(x) seq(0, x[2], by = 0.2)) +
    scale_x_datetime(date_labels = "%d-%b", breaks = breaks, name = "Sampling Date") +
    scale_color_manual(values = pal, guide = "none") +
    scale_fill_manual(values = c("grey10", "grey90"), name = "Measured Positivity") +
    scale_linetype_manual(values = c(5, 1), name = "Modeled Values") +
    guides(fill = guide_legend(order = 1)) +
    theme(axis.text.x = element_text(angle = 60, vjust =0.7, hjust = 0.5, size = rel(0.75)), axis.ticks = element_blank(),
          panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), axis.title.x = element_text(),
          legend.position = "bottom")

  if (!interactive()) rm(all_dat_sum, dat_sum, outcomes, all_time_series, time_series)
  fig_time_series

}
