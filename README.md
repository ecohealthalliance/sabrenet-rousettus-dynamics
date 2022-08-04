
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sabrenet-rousettus-dynamics

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6637927)](https://doi.org/10.5281/zenodo.6637927)
[![License (for code):
MIT](https://img.shields.io/badge/License%20(for%20code)-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![License:
CC0-1.0](https://img.shields.io/badge/License%20(for%20data)-CC0_1.0-lightgrey.svg)](http://creativecommons.org/publicdomain/zero/1.0/)
[![License:
CC-BY-4.0](https://img.shields.io/badge/License%20(for%20text)-CC_BY_4.0-blue.svg)](http://creativecommons.org/publicdomain/zero/1.0/)

This repository contains data and R code which are supplements to:

*Viral maintenance and excretion dynamics of coronaviruses within an
Egyptian rousette fruit bat colony- considerations for spillover*, by
Geldenhuys et al. (2022)

Please cite that paper, and/or the Zenodo data reference
(<a href="https://doi.org/######" class="uri">https://doi.org/######</a>)
when using data or referring to this study.

## Repository Structure and Reproducibility

-   `data/` contains data from the study and a data dictionary
    describing all variables.
-   `R/` contains functions used in this analysis.
-   `reports/` contains literate code for R Markdown reports generated
    in the analysis
-   `outputs/` contains compiled reports and figures.
-   This project uses the
    [{targets}](https://wlandau.github.io/targets-manual/) framework to
    organize build steps for analysis pipeline. The steps are defined in
    the `_targets.R` file and the workflow can be executed by running
    `run.R` via `source("run.R")` in your R terminal or `Rscript run.R`
    in your system shell. The schematic figure below summarizes the
    steps. (The figure is generated using `mermaid.js` syntax and should
    display as a graph on GitHub. It can also be viewed by pasting the
    code into <https://mermaid.live>.)

``` mermaid
graph LR
subgraph Project Workflow
    x7a7f43da56388c67(["fig_bat_demographics"]):::queued --> x57a7f9cc118d43ce(["fig_bat_demographics_file"]):::queued
    xba784c3a136c631a(["dat_cleaned"]):::queued --> x7a7f43da56388c67(["fig_bat_demographics"]):::queued
    xba784c3a136c631a(["dat_cleaned"]):::queued --> xf22c729b71100575(["time_series"]):::queued
    x4a5cfaffa1d0e789(["dat_prepped"]):::queued --> xf22c729b71100575(["time_series"]):::queued
    x89c8b8fe66c39f8c(["gam_posterior"]):::queued --> xf22c729b71100575(["time_series"]):::queued
    x023bc1d70802c4e1(["multinomial_model"]):::queued --> xf22c729b71100575(["time_series"]):::queued
    x6cf5d6dc2e05a667(["dat_bat"]):::queued --> xba784c3a136c631a(["dat_cleaned"]):::queued
    x733041ef94e8d4a9(["dat_fec"]):::queued --> xba784c3a136c631a(["dat_cleaned"]):::queued
    xc2117931d245afce(["dat_xls"]):::skipped --> x733041ef94e8d4a9(["dat_fec"]):::queued
    xe05634441492d438(["dat_captures"]):::skipped --> x370906d20a0ac4c5(["captures_cleaned"]):::queued
    x89c8b8fe66c39f8c(["gam_posterior"]):::queued --> x48dd96190e644d8f(["posterior_stats"]):::queued
    x023bc1d70802c4e1(["multinomial_model"]):::queued --> x89c8b8fe66c39f8c(["gam_posterior"]):::queued
    xc2117931d245afce(["dat_xls"]):::skipped --> x6cf5d6dc2e05a667(["dat_bat"]):::queued
    x89c8b8fe66c39f8c(["gam_posterior"]):::queued --> x1e7dc8900c0252af(["model_diagnostics"]):::queued
    x023bc1d70802c4e1(["multinomial_model"]):::queued --> x1e7dc8900c0252af(["model_diagnostics"]):::queued
    x48dd96190e644d8f(["posterior_stats"]):::queued --> x1e7dc8900c0252af(["model_diagnostics"]):::queued
    xb354ea9be719f8bb(["captures_xls"]):::skipped --> xe05634441492d438(["dat_captures"]):::skipped
    x4a5cfaffa1d0e789(["dat_prepped"]):::queued --> xe3a4a17736576ba6(["fig_time_series"]):::queued
    xf22c729b71100575(["time_series"]):::queued --> xe3a4a17736576ba6(["fig_time_series"]):::queued
    xba784c3a136c631a(["dat_cleaned"]):::queued --> xdb44d218f76593df(["dat_cleaned_csv"]):::queued
    x4a5cfaffa1d0e789(["dat_prepped"]):::queued --> x023bc1d70802c4e1(["multinomial_model"]):::queued
    x835f456db5951df5(["all_plot_files"]):::queued --> xbda0fd67e19f73bc(["outputs_readme"]):::queued
    x07b56d1fbca21cf3(["summarized_quantities"]):::skipped --> xbda0fd67e19f73bc(["outputs_readme"]):::queued
    x57a7f9cc118d43ce(["fig_bat_demographics_file"]):::queued --> x835f456db5951df5(["all_plot_files"]):::queued
    x650683ca19a5d319(["fig_time_series_file"]):::queued --> x835f456db5951df5(["all_plot_files"]):::queued
    xe3a4a17736576ba6(["fig_time_series"]):::queued --> x650683ca19a5d319(["fig_time_series_file"]):::queued
    xba784c3a136c631a(["dat_cleaned"]):::queued --> x4a5cfaffa1d0e789(["dat_prepped"]):::queued
  end
linkStyle 0 stroke-width:0px;
```

-   This project requires R version 4.2.1 (2022-06-23). This project
    uses the [{renv}](https://rstudio.github.io/renv/) framework to
    record R package dependencies and versions. Packages and versions
    used are recorded in `renv.lock` and code used to manage
    dependencies is in `renv/` and other files in the root project
    directory. On starting an R session in the working directory, run
    `renv::restore()` to install R package dependencies.
-   The package also requires
    [`cmdstan`](https://mc-stan.org/users/interfaces/cmdstan) to be
    installed. (Version 2.29.2 was used). If not already installed, run
    `cmdstanr::install_cmdstan(version = "2.29.2")` after
    `renv::restore()`
