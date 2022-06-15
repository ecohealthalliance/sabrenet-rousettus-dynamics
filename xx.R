library(curl)
library(readr)
h <- curl::new_handle()
h <- curl::handle_setheaders(h, Authorization = paste(Sys.getenv("DOLTHUB_TOKEN")))
con <- curl::curl("https://www.dolthub.com/csv/ecohealthalliance/dtra_ml_db/main/disease_key", handle = h)
dat <- readr::read_csv(con)
dat

