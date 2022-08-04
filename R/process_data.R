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
  dat_prepped_0 <- dat_cleaned |>
    filter(is.na(clade) | clade != 4) |>
    mutate(vir = if_else(is.na(genus), NA_character_, paste(genus, clade, sep = "-")),
           outcome = if_else(is.na(vir), 0L, as.integer(as.factor(vir))),
           day = as.numeric(date_collected - min(date_collected), "days"),
           day_of_year = lubridate::yday(date_collected),
           week = lubridate::round_date(date_collected, "week"),
           sample_type = fct_recode(sample_type, Fecal = "Faecal pool (x3)", Rectal = "Rectal swab"),
           fmi_kg_m2 = coalesce(fmi_kg_m2, mean_fmi),
           dummy_rectal = ordered(as.integer(sample_type == "Rectal")),
           demo_group = paste(gender, age, reproductive_condition)) |>
    mutate(across(c(gender, age, demo_group), \(x) if_else(sample_type == "Rectal", x, "NA"))) |>
    mutate(vir = c(NA_character_, "Novel Alpha-Cov", "HKU9-related Beta-CoV","Novel Beta-CoV")[outcome + 1]) |>
    mutate(across(c(sample_type, gender, age, demo_group, vir), as.factor)) |>
    group_by(week) |>
    mutate(frac_subadult = sum(age == "SA", na.rm = TRUE)/sum(!is.na(age))) |>
    mutate(dummy_any_rectal = as.numeric(any(sample_type == "Rectal"))) |>
    ungroup() |>
    mutate(frac_subadult = if_else(dummy_any_rectal == 0, mean(unique(frac_subadult[dummy_any_rectal == 1])), frac_subadult)) |>
    mutate(reproductive_condition = coalesce(reproductive_condition, "None") |>
             fct_recode(None = "Not pregnant", None = "Not scrotal") |>
             fct_relevel("None")) |>
    select(date = date_collected, day, day_of_year, week, sample_type, frac_subadult, dummy_rectal, dummy_any_rectal, gender, age, demo_group, reproductive_condition, fmi_kg_m2, cov_detected, vir, outcome)

  dat_prepped <- dat_prepped_0 |>
    mutate(gender_age = as.factor(if_else(sample_type == "Rectal", paste(gender, age, sep = "-"), "NA")),
           dummy_repro = ordered(if_else(reproductive_condition == "None", 0, 1)))

  dat_prepped
}
