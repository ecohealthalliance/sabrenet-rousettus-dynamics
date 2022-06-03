
# Load packages/functions --------
# Been having an issue where tidyverse won't load unless Matrix is loaded first
suppressPackageStartupMessages(source('packages.R'))

for (r_file in list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)) try(source(r_file))

tar_option_set(
  resources = tar_resources(
    qs = tar_resources_qs(preset = "fast")),
  format = "qs"
)

data_targets <- tar_plan(
  tar_file(dat_xls, "data/2021-10_ Supplementary datasets.xlsx"),
  dat_fec = read_xlsx(dat_xls, sheet = "Dataset 1", skip = 1),
  dat_bat = read_xlsx(dat_xls, sheet = "Dataset 2", skip = 1),
  dat_prepped = process_data(dat_fec, dat_bat)
)

analysis_targets <- tar_plan(

)

plot_targets <- tar_plan(
)

output_targets <- tar_plan(
#  tar_file(corrplot_file,
#           ggsave("outputs/bas_corrplot.png", bas_corrplot,
#                  dpi = 300, units = "in", width = 6, height = ,
#                  bg = "white")),
  tar_render(readme,
             path = "README.Rmd",
             output_dir = here::here())
)

list(
  data_targets,
  analysis_targets,
  plot_targets,
  output_targets
)
