
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sabrenet-rousettus-dynamics

\#[![DOI](https://zenodo.org/badge/DOI/#######)](https://doi.org/########)
[![License (for code):
MIT](https://img.shields.io/badge/License%20(for%20code)-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![License:
CC0-1.0](https://img.shields.io/badge/License%20(for%20data)-CC0_1.0-lightgrey.svg)](http://creativecommons.org/publicdomain/zero/1.0/)
[![License:
CC-BY-4.0](https://img.shields.io/badge/License%20(for%20text)-CC-BY-4.0-lightgrey.svg)](http://creativecommons.org/publicdomain/zero/1.0/)

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
    steps. (It uses `mermaid.js` syntax and should display as a graph on
    GitHub. It can also we viewed by pasting the code into
    <https://mermaid.live>.)

``` mermaid
graph LR
  subgraph Graph
    xc2117931d245afce(["dat_xls"]):::queued --> x733041ef94e8d4a9(["dat_fec"]):::queued
    xc2117931d245afce(["dat_xls"]):::queued --> x6cf5d6dc2e05a667(["dat_bat"]):::queued
    x6cf5d6dc2e05a667(["dat_bat"]):::queued --> x4a5cfaffa1d0e789(["dat_prepped"]):::queued
    x733041ef94e8d4a9(["dat_fec"]):::queued --> x4a5cfaffa1d0e789(["dat_prepped"]):::queued
    x3f8f3f89b15a4a7c>"process_data"]:::uptodate --> x4a5cfaffa1d0e789(["dat_prepped"]):::queued
    x6e52cb0f1668cc22(["readme"]):::started --> x6e52cb0f1668cc22(["readme"]):::started
    xf7544390134f4647{{"plot_targets"}}:::uptodate --> xf7544390134f4647{{"plot_targets"}}:::uptodate
    xad15c442a1533efe{{"output_targets"}}:::outdated --> xad15c442a1533efe{{"output_targets"}}:::outdated
    x6a7cdb164c636498{{"analysis_targets"}}:::uptodate --> x6a7cdb164c636498{{"analysis_targets"}}:::uptodate
    xbea903ca6e7865e7{{"user_rprof"}}:::outdated --> xbea903ca6e7865e7{{"user_rprof"}}:::outdated
    x7f1f0b8d59d2dfab{{"r_file"}}:::uptodate --> x7f1f0b8d59d2dfab{{"r_file"}}:::uptodate
    x61c99aa4be265faa{{"data_targets"}}:::outdated --> x61c99aa4be265faa{{"data_targets"}}:::outdated
  end
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
        installed. Version 2.29.2 is used. If not already installed, run
        `cmdstanr::install_cmdstan(version = "2.29.2")` after
        `renv::restore()`
