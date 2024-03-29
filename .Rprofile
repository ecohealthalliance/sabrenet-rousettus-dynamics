if (file.exists(".env")) {
  try(readRenviron(".env"))
}

options(
  repos = c(RSPM = "https://packagemanager.rstudio.com/all/latest",
            CRAN = "https://cran.rstudio.com/",
            INLA = "https://inla.r-inla-download.org/R/testing"),
  pkgType = "binary",
  Ncpus = 3,
  renv.config.auto.snapshot = TRUE, ## Attempt to keep renv.lock updated automatically
  renv.config.rspm.enabled = TRUE, ## Use RStudio Package manager for pre-built package binaries
  renv.config.install.shortcuts = TRUE, ## Use the existing local library to fetch copies of packages for renv
  renv.config.cache.enabled = TRUE,   ## Use the renv build cache to speed up install times
  renv.config.cache.symlinks = TRUE,  ## Keep full copies of packages locally than symlinks to make the project portable in/out of containers
  renv.config.install.transactional = FALSE,
  renv.config.synchronized.check = FALSE,
  renv.config.updates.parallel = TRUE,
#  renv.config.pak.enabled = TRUE,
  renv.config.rspm.enabled = TRUE,

  mc.cores = min(parallel::detectCores(), 4)

)

if(Sys.getenv("USE_CAPSULE") %in% c("1", "TRUE", "true")) {
  if (interactive() && file.exists("renv.lock")) {
    message("renv library not loaded (found env var USE_CAPSULE=", Sys.getenv("USE_CAPSULE"), "). Use `capsule` functions (see https://github.com/MilesMcBain/capsule)")
    capsule::whinge()
  }
} else {
  source("renv/activate.R")
}

# Use the local user's .Rprofile when interactive.
# Good for keeping local preferences, but not always reproducible.
user_rprof <- Sys.getenv("R_PROFILE_USER")
if (is.na(user_rprof) || is.null(user_rprof) || user_rprof == "") {
  user_rprof <- normalizePath("~/.Rprofile", mustWork = FALSE)
}
if(interactive() && file.exists(user_rprof)) {
  source(user_rprof)
}
rm(user_rprof)

# If project packages have conflicts define them here
if(requireNamespace("conflicted", quietly = TRUE)) {
  conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("count", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("select", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("geom_rug", "ggplot2", quiet = TRUE)
  conflicted::conflict_prefer("set_names", "magrittr", quiet = TRUE)
  conflicted::conflict_prefer("View", "utils", quiet = TRUE)
}
