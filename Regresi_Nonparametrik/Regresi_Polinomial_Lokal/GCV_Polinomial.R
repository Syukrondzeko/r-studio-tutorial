# ==============================================================================
# Regresi polinomial lokal: fungsi estimasi dan pemilihan bandwidth dengan GCV
# ------------------------------------------------------------------------------
# Membutuhkan ker() (helpers/kernel.R) dan MPL() (helpers/invers_matrix.R)
#
# Pada regresi polinomial lokal, untuk SETIAP titik X[i] dibentuk polinomial
# berbobot yang berpusat di X[i]; estimasi m(X[i]) adalah intersep (beta0) dari
# polinomial lokal tersebut. Karena itu matriks hat dibentuk baris demi baris.
# ==============================================================================

# Parameter polinomial lokal derajat p di satu titik x0 dengan bandwidth h
beta_lokal <- function(x0, X, Y, p, h) {
  matrix_X <- outer(X - x0, 0:p, `^`)
  W <- diag(ker((X - x0) / h))
  L <- MPL(t(matrix_X) %*% W %*% matrix_X) %*% t(matrix_X) %*% W
  list(beta = L %*% Y, L = L)
}

# Matriks hat (n x n) regresi polinomial lokal derajat p dengan bandwidth h
hat_lokal <- function(X, p, h) {
  n <- length(X)
  H <- matrix(0, n, n)
  for (i in 1:n) {
    H[i, ] <- beta_lokal(X[i], X, rep(0, n), p, h)$L[1, ]   # baris beta0
  }
  H
}

# gcv_LocalPolynomial(X, Y, band, width, p, by)
#   band, width : batas bawah dan atas bandwidth h
#   p           : derajat polinomial
#   by          : langkah pencarian h
# Mengembalikan list: h.opt, tabel (h, GCV, MSE), top10
gcv_LocalPolynomial <- function(X, Y, band, width, p, by = 1) {
  n <- length(X)
  h <- seq(band, width, by = by)
  GCV <- MSE <- numeric(length(h))
  for (m in seq_along(h)) {
    H_hat <- hat_lokal(X, p, h[m])
    m_hat <- H_hat %*% Y
    MSE[m] <- sum((Y - m_hat)^2) / n
    GCV[m] <- MSE[m] / (1 - sum(diag(H_hat)) / n)^2
  }
  R <- cbind(h = h, GCV = GCV, MSE = MSE)
  S <- R[order(R[, "GCV"]), , drop = FALSE][1:min(10, nrow(R)), , drop = FALSE]
  cat("Running program untuk derajat polinomial p =", p, "\n")
  cat("=========================================", "\n")
  print(S)
  cat("=========================================", "\n")
  invisible(list(h.opt = S[1, "h"], tabel = R, top10 = S))
}
