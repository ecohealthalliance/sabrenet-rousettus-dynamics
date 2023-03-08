
# Load packages/functions --------
# Been having an issue where tidyverse won't load unless Matrix is loaded first
suppressPackageStartupMessages(source("packages.R"))
targets::tar_source()

tar_option_set(
  resources = tar_resources(
    qs = tar_resources_qs(preset = "fast")),
  format = "qs"
)

tar_option_set(debug = "plotfiles")

data_targets <- tar_plan(
  tar_file(dat_xls, "data/2022-06; Supplementary datasets.xlsx"),
  tar_file(captures_xls, "data/Captures per month.xlsx"),
  dat_captures = readxl::read_xlsx(captures_xls),
  dat_fec = readxl::read_xlsx(dat_xls, sheet = "Dataset 1", skip = 1),
  dat_bat = readxl::read_xlsx(dat_xls, sheet = "Dataset 2", skip = 1),
  captures_cleaned = clean_captures(dat_captures),
  dat_cleaned = clean_data(dat_fec, dat_bat),
  tar_file(model_terms_table_file, "data/model-terms-table.csv"),
  model_terms_table = readr::read_csv(model_terms_table_file)
)

analysis_targets <- tar_plan(
  dat_prepped = prep_data(dat_cleaned),
  tar_target(multinomial_model, fit_multinomial_model(dat_prepped), cue = tar_cue("thorough")),
  tar_target(gam_posterior, sample_gam_posterior(multinomial_model, chains = 4,
                                                 burn = 1250, ns = 13750, thin = 100, rw.scale = 0.1),
             cue = tar_cue("thorough")),
  posterior_stats = calc_posterior_stats(gam_posterior),
  time_series = calc_time_series(dat_cleaned, dat_prepped, multinomial_model, gam_posterior),
  raw_prev = calc_raw_prev(dat_prepped),
  model_prev = calc_model_prev(dat_prepped, multinomial_model, gam_posterior),
  table_fmi_demo = tabulate_fmi_demo(dat_prepped),
  table_gam_summary = tabulate_gam_summary(dat_prepped, multinomial_model, gam_posterior, model_terms_table),
  partial_effect_plots = make_partial_effect_plots(multinomial_model),
  flextable_gam_summary = make_flextable_gam_summary(table_gam_summary)
)

plot_targets <- tar_plan(
  fig_bat_demographics =    structure(plot_bat_demographics(dat_prepped, dat_captures), fig.width = 8, fig.height = 4.5),
  fig_fmi_demo =            structure(plot_fmi_demo(dat_prepped), fig.width = 8, fig.height = 8),
  fig_time_series =         structure(plot_time_series(dat_prepped, time_series), fig.width = 8, fig.height = 6),
  fig_fmi_demo_timeseries = structure(plot_fmi_demo_timeseries(dat_prepped), fig.width = 8, fig.height = 6),
  fig_pos_demo_timeseries = structure(plot_positivity_demo_timeseries(dat_prepped), fig.width = 8, fig.height = 4),
  fig_fmi_effects =         structure(plot_fmi_effects(dat_prepped, multinomial_model, gam_posterior), fig.width = 8, fig.height = 4.5),
  fig_fmi_demo_effects =    structure(plot_fmi_demo_effects(dat_prepped), fig.width = 8, fig.height = 6)
)


plot_file_targets <- tar_plan(
  tar_combine(allplots, plot_targets, command = vctrs::vec_c(list(!!!.x))),
  tar_file(png_plots, ggsave(
    paste0("outputs/", names(allplots), ".png"), allplots[[1]],
    units = "in", bg = "white",
    width = attr(allplots[[1]], "fig.width"), height = attr(allplots[[1]], "fig.height")),
    pattern = map(allplots)),
  tar_file(svg_plots, ggsave(
    paste0("outputs/", names(allplots), ".svg"), allplots[[1]],
    units = "in", bg = "white",
    width = attr(allplots[[1]], "fig.width"), height = attr(allplots[[1]], "fig.height")),
    pattern = map(allplots)),
  tar_file(partial_effect_plots_png, c(
    ggsave("outputs/partial_effect_plots_1.png", partial_effect_plots[[1]],
           scale = 2, width = 6.5, height = 7.5),
    ggsave("outputs/partial_effect_plots_2.png", partial_effect_plots[[2]],
           scale = 2, width = 6.5, height = 7.5)
  )),
  tar_file(partial_effect_plots_svg, c(
    ggsave("outputs/partial_effect_plots_1.svg", partial_effect_plots[[1]],
           scale = 2, width = 6.5, height = 9),
    ggsave("outputs/partial_effect_plots_2.svg", partial_effect_plots[[2]],
           scale = 2, width = 6.5, height = 7.2)
  ))
)

output_targets <- tar_plan(
  tar_file(plan_targets, "_targets.R"), # make the plan a target so README updates
  tar_render(readme,
             path = "README.Rmd"),
  tar_render(model_diagnostics,
             path = "reports/model_diagnostics.Rmd"),
  tar_file(gam_summary_docx,
           flextable::save_as_docx(values = flextable_gam_summary,
                        path = "outputs/gam_summary.docx",
                        pr_section = officer::prop_section(
                          page_size = officer::page_size(orient = "landscape")
                        ))),
  tar_file(dat_cleaned_csv, {
    write_csv(dat_cleaned, "outputs/data_prepped.csv")
    "outputs/data_prepped.csv"
  }),

  summarized_quantities = summarize_quantities(),
  tar_render(outputs_readme, path = "outputs/README.Rmd"),
)

list(
  data_targets,
  analysis_targets,
  plot_targets,
  plot_file_targets,
  output_targets
)
