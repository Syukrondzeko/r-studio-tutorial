# ==============================================================================
# Pemilihan titik knot optimal regresi spline truncated (1 titik knot) dengan GCV
# ------------------------------------------------------------------------------
# gcv1(y, x, m, l): spline orde m, kandidat knot dari min(x)+1 sampai max(x)-1
#                   dengan langkah l. Mengembalikan 10 knot dengan GCV terkecil.
# Membutuhkan MPL() (helpers/invers_matrix.R) dan trun() (helpers/truncated.R)
# ==============================================================================

gcv1 <- function(y, x, m, l) {
  k <- seq(min(x) + 1, max(x) - 1, l)
  v <- length(k)
  n <- length(y)
  I <- diag(n)
  Gcv <- numeric(v)
  Mse <- numeric(v)
  for (j in 1:v) {
    w <- matrix(0, ncol = m + 1, nrow = n)
    for (i in 1:m) w[, i] <- x^(i - 1)
    w[, m + 1] <- trun(x, k[j], m - 1)
    z    <- MPL(t(w) %*% w)
    beta <- z %*% (t(w) %*% y)
    h    <- w %*% z %*% t(w)
    mu   <- w %*% beta
    MSE  <- sum((y - mu)^2) / n
    Gcv[j] <- (n^2 * MSE) / (sum(diag(I - h)))^2
    Mse[j] <- MSE
  }
  R <- cbind(knot = k, GCV = Gcv, MSE = Mse)
  S <- R[order(R[, "GCV"]), , drop = FALSE][1:min(10, v), , drop = FALSE]
  cat("Untuk spline orde", m, "dengan 1 titik knot, diperoleh knot optimal =", S[1, 1],
      "dengan GCV minimum =", S[1, 2], "dan MSE =", S[1, 3])
  cat("\nBerikut 10 nilai GCV terkecil, nilai MSE dan letak titik knotnya:\n")
  S
}
