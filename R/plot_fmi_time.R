#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dat_prepped
#' @return
#' @author 'Noam Ross'
#' @export
plot_fmi_time <- function(dat_prepped) {
  fmi_all <- dat_prepped |>
    filter(sample_type == "Rectal") |>
    group_by(date) |>
    ggdist::mean_qi(fmi_kg_m2)

  fmi_group <- dat_prepped |>
    filter(sample_type == "Rectal") |>
    group_by(date, age, gender) |>
    ggdist::mean_qi(fmi_kg_m2)

  gplot <- ggplot(fmi_group, aes(x = date, y = fmi_kg_m2, ymin = .lower, ymax = .upper)) +
    geom_smooth(data = fmi_all, col="grey30") +
    geom_errorbar(mapping = aes( color = paste(age, gender, sep ="-"))) +
    geom_point(mapping = aes( color = paste(age, gender, sep ="-"))) +
    facet_wrap(~paste(age, gender, sep ="-")) +
    geom_smooth(mapping = aes( color = paste(age, gender, sep ="-")))

  rm(dat_prepped)
  gplot

}
