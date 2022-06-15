
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
Objects([""Objects""]) --- Functions>""Functions""]
subgraph Project Workflow
    x7a7f43da56388c67(["fig_bat_demographics"]):::queued --> x57a7f9cc118d43ce(["fig_bat_demographics_file"]):::queued
    xba784c3a136c631a(["dat_cleaned"]):::queued --> x7a7f43da56388c67(["fig_bat_demographics"]):::queued
    x923e5d3f7effdb97>"plot_bat_demographics"]:::uptodate --> x7a7f43da56388c67(["fig_bat_demographics"]):::queued
    x3541a3d3c2c93003>"calc_time_series"]:::uptodate --> xf22c729b71100575(["time_series"]):::queued
    xba784c3a136c631a(["dat_cleaned"]):::queued --> xf22c729b71100575(["time_series"]):::queued
    x4a5cfaffa1d0e789(["dat_prepped"]):::queued --> xf22c729b71100575(["time_series"]):::queued
    x89c8b8fe66c39f8c(["gam_posterior"]):::queued --> xf22c729b71100575(["time_series"]):::queued
    x023bc1d70802c4e1(["multinomial_model"]):::queued --> xf22c729b71100575(["time_series"]):::queued
    xba784c3a136c631a(["dat_cleaned"]):::queued --> xcdebc0583bb5c42e(["fig_size_demo"]):::queued
    xdf8b170c766e2d6b>"plot_size_demo"]:::uptodate --> xcdebc0583bb5c42e(["fig_size_demo"]):::queued
    xdc4c0b12c3eff7c3>"clean_data"]:::uptodate --> xba784c3a136c631a(["dat_cleaned"]):::queued
    x6cf5d6dc2e05a667(["dat_bat"]):::queued --> xba784c3a136c631a(["dat_cleaned"]):::queued
    x733041ef94e8d4a9(["dat_fec"]):::queued --> xba784c3a136c631a(["dat_cleaned"]):::queued
    x4a5cfaffa1d0e789(["dat_prepped"]):::queued --> x743d14db79d088a5(["fig_fmi_time"]):::queued
    x81e3813389f7c111>"plot_fmi_time"]:::uptodate --> x743d14db79d088a5(["fig_fmi_time"]):::queued
    xc2117931d245afce(["dat_xls"]):::queued --> x733041ef94e8d4a9(["dat_fec"]):::queued
    x23873f9d63594e23(["fig_fmi_demo"]):::queued --> x8fc58a4d202a711a(["fig_fmi_demo_file"]):::queued
    xc0afa1ca6e3a3ecf>"calc_posterior_stats"]:::uptodate --> x48dd96190e644d8f(["posterior_stats"]):::queued
    x89c8b8fe66c39f8c(["gam_posterior"]):::queued --> x48dd96190e644d8f(["posterior_stats"]):::queued
    x023bc1d70802c4e1(["multinomial_model"]):::queued --> x89c8b8fe66c39f8c(["gam_posterior"]):::queued
    x8ff1740672def84e>"sample_gam_posterior"]:::uptodate --> x89c8b8fe66c39f8c(["gam_posterior"]):::queued
    xc2117931d245afce(["dat_xls"]):::queued --> x6cf5d6dc2e05a667(["dat_bat"]):::queued
    xcdebc0583bb5c42e(["fig_size_demo"]):::queued --> x6c01430c132507c0(["fig_size_demo_file"]):::queued
    x89c8b8fe66c39f8c(["gam_posterior"]):::queued --> x1e7dc8900c0252af(["model_diagnostics"]):::queued
    x023bc1d70802c4e1(["multinomial_model"]):::queued --> x1e7dc8900c0252af(["model_diagnostics"]):::queued
    x48dd96190e644d8f(["posterior_stats"]):::queued --> x1e7dc8900c0252af(["model_diagnostics"]):::queued
    x4a5cfaffa1d0e789(["dat_prepped"]):::queued --> x37da1c8de8c2ccbe(["fig_fmi_effects"]):::queued
    x89c8b8fe66c39f8c(["gam_posterior"]):::queued --> x37da1c8de8c2ccbe(["fig_fmi_effects"]):::queued
    x023bc1d70802c4e1(["multinomial_model"]):::queued --> x37da1c8de8c2ccbe(["fig_fmi_effects"]):::queued
    xd5ee960dd8f86a23>"plot_fmi_effects"]:::uptodate --> x37da1c8de8c2ccbe(["fig_fmi_effects"]):::queued
    x4a5cfaffa1d0e789(["dat_prepped"]):::queued --> xe3a4a17736576ba6(["fig_time_series"]):::queued
    x6ee5b97ea4d7b6f2>"plot_time_series"]:::uptodate --> xe3a4a17736576ba6(["fig_time_series"]):::queued
    xf22c729b71100575(["time_series"]):::queued --> xe3a4a17736576ba6(["fig_time_series"]):::queued
    xba784c3a136c631a(["dat_cleaned"]):::queued --> xdb44d218f76593df(["dat_cleaned_csv"]):::queued
    xba784c3a136c631a(["dat_cleaned"]):::queued --> x23873f9d63594e23(["fig_fmi_demo"]):::queued
    x0f821fb38c59b6f4>"plot_fmi_demo"]:::uptodate --> x23873f9d63594e23(["fig_fmi_demo"]):::queued
    x4a5cfaffa1d0e789(["dat_prepped"]):::queued --> x023bc1d70802c4e1(["multinomial_model"]):::queued
    x98c1037972a8a636>"fit_multinomial_model"]:::uptodate --> x023bc1d70802c4e1(["multinomial_model"]):::queued
    x37da1c8de8c2ccbe(["fig_fmi_effects"]):::queued --> x942eeb410ea37982(["fig_fmi_effects_file"]):::queued
    xe3a4a17736576ba6(["fig_time_series"]):::queued --> x650683ca19a5d319(["fig_time_series_file"]):::queued
    xba784c3a136c631a(["dat_cleaned"]):::queued --> x4a5cfaffa1d0e789(["dat_prepped"]):::queued
    x9369f9c58212db0f>"prep_data"]:::uptodate --> x4a5cfaffa1d0e789(["dat_prepped"]):::queued
  end
linkStyle 0 stroke-width:0px;
```

-   This project requires R version 4.2.0 (2022-04-22). This project
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
