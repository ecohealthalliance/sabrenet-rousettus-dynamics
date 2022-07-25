
# Load packages/functions --------
# Been having an issue where tidyverse won't load unless Matrix is loaded first
suppressPackageStartupMessages(source('packages.R'))

for (r_file in list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)) try(source(r_file))
rm(r_file)
tar_option_set(
  resources = tar_resources(
    qs = tar_resources_qs(preset = "fast")),
  format = "qs"
)

#tar_option_set(debug = "fig_time_series")

data_targets <- tar_plan(
  tar_file(dat_xls, "data/2022-06; Supplementary datasets.xlsx"),
  tar_file(captures_xls, "data/Captures per month.xlsx"),
  dat_captures = readxl::read_xlsx(captures_xls),
  dat_fec = readxl::read_xlsx(dat_xls, sheet = "Dataset 1", skip = 1),
  dat_bat = readxl::read_xlsx(dat_xls, sheet = "Dataset 2", skip = 1),
  captures_cleaned = clean_captures(dat_captures),
  dat_cleaned = clean_data(dat_fec, dat_bat)

)

analysis_targets <- tar_plan(
  dat_prepped = prep_data(dat_cleaned),
  multinomial_model = fit_multinomial_model(dat_prepped),
  gam_posterior = sample_gam_posterior(multinomial_model, chains = 4, cores = 2,
                                       ns = 20000, thin = 100, rw.scale = 0.05),
  posterior_stats = calc_posterior_stats(gam_posterior),
  time_series = calc_time_series(dat_cleaned, dat_prepped, multinomial_model, gam_posterior)
)

plot_targets <- tar_plan(
  fig_bat_demographics = plot_bat_demographics(dat_cleaned),
  fig_fmi_demo = plot_fmi_demo(dat_cleaned),
  fig_size_demo = plot_size_demo(dat_cleaned),

  fig_time_series = plot_time_series(dat_prepped, time_series),
  fig_fmi_time = plot_fmi_time(dat_prepped),
  fig_fmi_effects = plot_fmi_effects(dat_prepped, multinomial_model, gam_posterior)
)

output_targets <- tar_plan(
  tar_file(plan_targets, "_targets.R"), # make the plan a target so README updates
  tar_render(readme,
             path = "README.Rmd"),
  tar_render(model_diagnostics,
             path = "reports/model_diagnostics.Rmd"),
  tar_file(dat_cleaned_csv, {
    write_csv(dat_cleaned, "outputs/data_prepped.csv")
    "outputs/data_prepped.csv"
  }),
  tar_file(fig_bat_demographics_file,
           ggsave("outputs/fig_bat_demographics.png", fig_bat_demographics,
                  dpi = 300, units = "in", width = 8, height = 4.5, bg = "white")),
  tar_file(fig_time_series_file,
           ggsave("outputs/fig_time_series.png", fig_time_series,
                  dpi = 300, units = "in", width = 8, height = 6, bg = "white")),
  tar_file(fig_fmi_effects_file,
           ggsave("outputs/fig_fmi_effects.png", fig_fmi_effects,
                  dpi = 300, units = "in", width = 8, height = 4, bg = "white")),
  tar_file(fig_fmi_demo_file,
           ggsave("outputs/fig_fmi_demo.png", fig_fmi_demo,
                  dpi = 300, units = "in", width = 8, height = 6, bg = "white")),
  tar_file(fig_size_demo_file,
           ggsave("outputs/fig_size_demo.png", fig_size_demo,
                  dpi = 300, units = "in", width = 8, height = 6, bg = "white"))
)

list(
  data_targets,
  analysis_targets,
  plot_targets,
  output_targets
)
