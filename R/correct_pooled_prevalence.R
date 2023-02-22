# Pooled prevalence correction for pools of equal size
# From Burrows PM (1987). Improved estimation of pathogen transmission rates by
# group testing. Phytopathology 77 363–365.
correct_pooled_prevalence <- function(num_pos_pools, num_pools, pool_size) {
  x <- num_pos_pools
  n <- num_pools
  m <- pool_size

  a <- 0.5*(m-1)/m

  p_hat <- 1 - ((n - x + a)/(n + a))^(1/m)

  p_hat
}

