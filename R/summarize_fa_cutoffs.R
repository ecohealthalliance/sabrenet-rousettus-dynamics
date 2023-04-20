#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dat_cleaned
#' @return
#' @author 'Noam Ross'
#' @export
summarize_fa_cutoffs <- function(dat_cleaned) {

  test_cutoffs <- c(89, 90, 94)


  dat <- dat_cleaned |>
    filter(sample_type == "Rectal swab") |>
    mutate(fa_89 = if_else(fa_mm >= 89, "> 89", "< 89")) |>
    mutate(teat_assessment = if_else(str_detect(decisions_based_on_data_sheets, "teat") |
                                       teat_assessment %in% c("Not recorded", "Not applicable"),
                                     NA_character_, teat_assessment)) |>
    select(sex = gender, age, fa_mm, fa_89, mass_g, reproductive_status, reproductive_condition, teat_assessment) |>
    mutate(sex = case_match(sex, "M" ~ "Male", "F" ~ "Female"))

  min_cutoff <- dat |>
    filter(!reproductive_condition %in% c("Not scrotal", "Not pregnant")) |>
    pull(fa_mm) |>
    min() |> floor()

  p1 <- ggplot(dat, aes(x = fa_mm, fill = paste(reproductive_condition))) +
    geom_histogram(binwidth = 1, boundary = 0, col = "white", linewidth = 0.25) +
    scale_x_continuous(breaks = seq(70, 105, by = 2), name = "Forearm Length (mm)") +
    scale_fill_discrete(name = "Reproductive Status") +
    facet_wrap(~sex, ncol = 1) +
    geom_vline(xintercept = c(min_cutoff, test_cutoffs), col = "black") +
    theme(legend.position = "bottom")

  p2 <- ggplot(dat, aes(x = fa_mm^2, y = mass_g, color = paste(reproductive_condition, age, sex))) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ 0 + x) +
    #scale_x_continuous(breaks = seq(70, 105, by = 2), name = "Forearm Length (mm)") +
    scale_color_discrete(name = "Reproductive Status") +
    facet_wrap(~sex, ncol = 1) +
    theme(legend.position = "bottom")
  #  geom_vline(xintercept = c(min_cutoff, test_cutoffs), col = "black")

  p1 + p2
}
