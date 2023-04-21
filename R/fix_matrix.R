#' Fix a matrix that is not positive definite to be so
#' Based on https://www.r-bloggers.com/2012/10/fixing-non-positive-definite-correlation-matrices-using-r-2/
#' and references therein
fix_matrix <- function(mat) {
  chol_err <- inherits(try(chol(mat), silent = TRUE), "try-error")
  while (chol_err) {
    eig <- eigen(mat)
    new_eigvals <- pmax(eig$values, 0)
    mat <- eig$vectors %*% diag(new_eigvals) %*% t(eig$vectors)
    chol_err <- inherits(try(chol(mat), silent = TRUE), "try-error")
  }
  return(mat)
}
