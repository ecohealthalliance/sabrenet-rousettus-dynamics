#' Fix a matrix that is not positive definite to be so
#' Based on https://www.r-bloggers.com/2012/10/fixing-non-positive-definite-correlation-matrices-using-r-2/
#' and references therein
fix_matrix <- function(mat) {
  ct <- chol_test(mat)
  while (ct) {
    eig <- eigen(mat)
    eig$values <- Re(eig$values); eig$vectors <- Re(eig$vectors)
    new_eigvals <- pmax(eig$values, 0)
    mat <- eig$vectors %*% diag(new_eigvals) %*% t(eig$vectors)
    ct <- chol_test(mat)
  }
  return(mat)
}

chol_test <- function(mat) {
  chol_try <- try(chol(mat), silent = TRUE)
  chol_err <- inherits(chol_try, "try-error")
  if(chol_err && !grepl("not positive definite", attr(chol_try, "condition")$message)) {
    stop(attr(chol_try, "message"))
  }
  return(chol_err)
}
