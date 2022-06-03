#' .. content for \description{} (no empty lines) ..
#'
#' Combine bat and fecal data into single data frame and clean for
#' modeling
#'
#' @title
#' @param dat_fec
#' @param dat_bat
#' @return
#' @author 'Noam Ross'
#' @export
process_data <- function(dat_fec, dat_bat) {
  dat_combined <- bind_rows(
  dat_fec |>
    clean_names() |>
    rename(sample_id = sample_number, cov_detected = coronavirus_detected),
  dat_bat |> clean_names() |>
    rename(sample_d = up, cov_detected = co_v_detected) |>
    select(-tatoo)
  )

  dat_cleaned <- dat_combined |>
    mutate(genus = stri_trans_tolower(genus))
  dat_prepped <- dat_cleaned
  dat_prepped
}
