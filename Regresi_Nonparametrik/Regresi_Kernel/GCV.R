# ==============================================================================
# Pemilihan bandwidth optimal regresi kernel (Nadaraya-Watson) dengan GCV
# ------------------------------------------------------------------------------
# gcv(X, Y, a, b, c): mencoba bandwidth h = a, a+c, ..., b
# Mengembalikan list:
#   h.opt : bandwidth dengan GCV minimum
#   tabel : matriks (h, GCV, MSE) untuk semua h
#   top10 : 10 nilai GCV terkecil
# Membutuhkan fungsi ker() dari Regresi_Nonparametrik/helpers/kernel.R
# ==============================================================================

gcv <- function(X, Y, a, b, c) {
  h <- seq(a, b, by = c)
  s <- length(h)
  n <- length(Y)
  GCV <- numeric(s)
  MSE <- numeric(s)
  for (m in 1:s) {
    Ghat <- numeric(n)
    hi   <- numeric(n)   # elemen diagonal matriks hat
    for (i in 1:n) {
      bobot  <- ker((X[i] - X) / h[m])
      faktor <- sum(bobot)
      Ghat[i] <- if (faktor == 0) 0 else sum(bobot * Y) / faktor
      hi[i]   <- ker(0) / faktor
    }
    RSS    <- sum((Y - Ghat)^2)
    MSE[m] <- RSS / n
    GCV[m] <- (n * RSS) / (n - sum(hi))^2
  }
  R <- cbind(h = h, GCV = GCV, MSE = MSE)
  S <- R[order(R[, "GCV"]), , drop = FALSE][1:min(10, s), , drop = FALSE]
  cat("\nh opt =", S[1, 1], "dengan GCV minimal =", S[1, 2], "dan MSE =", S[1, 3],
      "\nBerikut 10 nilai GCV terkecil dan MSE beserta nilai bandwidth h:\n")
  print(S)
  invisible(list(h.opt = S[1, 1], tabel = R, top10 = S))
}
