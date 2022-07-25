#' Combine bat and fecal data into single data frame and clean for use
#' modeling
clean_data <- function(dat_fec, dat_bat) {
  dat_combined <- bind_rows(
    dat_fec |>
      janitor::clean_names() |>
      rename(sample_id = sample_number, cov_detected = coronavirus_detected),
    dat_bat |> janitor::clean_names() |>
      rename(sample_d = up, cov_detected = co_v_detected) |>
      select(-tatoo)
  )

  dat_cleaned <- dat_combined |>
    mutate(genus = tolower(genus))
  dat_cleaned
}

#' Prepare data for modeling, removing NA values, creating dummy varibles,
#' converting to appropriate types, calculating derived variables (day of year, etc)
prep_data <- function(dat_cleaned) {
  mean_fmi <- dat_cleaned |>
    filter(!is.na(fmi_kg_m2), fmi_kg_m2 != 0) |>
    pull(fmi_kg_m2) |> mean()
  dat_prepped <- dat_cleaned |>
    filter(is.na(clade) | clade != 4) |>
    mutate(vir = if_else(is.na(genus), NA_character_, paste(genus, clade, sep = "-")),
           outcome = if_else(is.na(vir), 0L, as.integer(as.factor(vir))),
           day = as.numeric(date_collected - min(date_collected), "days"),
           day_of_year = lubridate::yday(date_collected),
           sample_type = fct_recode(sample_type, Fecal = "Faecal pool (x3)", Rectal = "Rectal swab"),
           fmi_kg_m2 = coalesce(fmi_kg_m2, mean_fmi),
           dummy_rectal = as.integer(sample_type == "Rectal"),
           demo_group = paste(gender, age, reproductive_condition)) |>
    mutate(across(c(gender, age, demo_group), \(x) if_else(sample_type == "Rectal", x, "NA"))) |>
    mutate(vir = c(NA_character_, "Novel Alpha-Cov", "HKU9-related Beta-CoV","Novel Beta-CoV")[outcome + 1]) |>
    mutate(across(c(sample_type, gender, age, demo_group, vir), as.factor)) |>
    group_by(date_collected) |>
    mutate(frac_subadult = sum(age == "SA", na.rm = TRUE)/sum(!is.na(age))) |>
    ungroup() |>
    select(date = date_collected, day, day_of_year, sample_type, frac_subadult, dummy_rectal, gender, age, demo_group, fmi_kg_m2, cov_detected, vir, outcome)

    dat_prepped
}
