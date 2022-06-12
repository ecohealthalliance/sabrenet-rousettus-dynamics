#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dat_prepped
#' @return
#' @author 'Noam Ross'
#' @export
plot_bat_demographics <- function(dat_cleaned) {

  dat <- dat_cleaned |>
    filter(sample_type == "Rectal swab") |>
    count(date_collected, gender, age, reproductive_condition) |>
    mutate(demo = paste(gender, reproductive_condition, sep = "-"),
           age = fct_recode(age,  Adults = "A", Subadults = "SA"))



  fig_bat_demographics <-
    ggplot(dat, aes(x = n, y = fct_rev(as.factor(date_collected)), fill = demo)) +
    geom_col(position = "stack", col = "black", lwd = 0.5) +
    facet_grid(~age) +
    scale_fill_brewer(type = "qual", name = "") +
    labs(
      y = "Collection Date",
      x = "Number of Bats Sampled"
    ) +
    theme(legend.position = c(0.90, 0.5),
          legend.background = element_rect(fill="white"),
          strip.background = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color="grey", size  = 0.25),
          axis.ticks = element_blank(),
          panel.ontop = TRUE
    )
  rm(dat_cleaned, dat)
  fig_bat_demographics
}

plot_fmi_demo <- function(dat_cleaned) {
  dat <- dat_cleaned |>
    filter(sample_type == "Rectal swab") |>
    mutate(demo = paste(gender, age, reproductive_condition, sep = "-")) |>
    mutate(demo = fct_reorder(demo, fmi_kg_m2))

  fig_fmi_demo <- ggplot(dat, aes(x = fmi_kg_m2, y = demo, fill = demo)) +
    ggridges::geom_density_ridges(
      scale = 0.9,
      jittered_points = TRUE,
      position = ggridges::position_points_jitter(width = 0.05, height = 0),
      point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7
    ) +
    scale_fill_viridis_d(name = "") +
    labs(
      y = "Demographic Group",
      x = "Forearm Mass Index (kg/m2)"
    )
  rm(dat, dat_cleaned)
  fig_fmi_demo
}

plot_size_demo <- function(dat_cleaned) {
  dat <- dat_cleaned |>
    filter(sample_type == "Rectal swab") |>
    mutate(demo = paste(gender, age, reproductive_condition, sep = "-")) |>
    mutate(demo = fct_reorder(demo, mass_g))

  fig_size_demo <- ggplot(dat, aes(x = mass_g, y = demo, fill = demo)) +
    ggridges::geom_density_ridges(
      scale = 0.9,
      jittered_points = TRUE,
      position = ggridges::position_points_jitter(width = 0.05, height = 0),
      point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7
    ) +
    scale_fill_viridis_d(name = "") +
    labs(
      y = "Demographic Group",
      x = "Mass (g)"
    )
  rm(dat, dat_cleaned)
  fig_size_demo
}