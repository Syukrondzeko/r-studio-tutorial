# ==============================================================================
# Pemilihan bandwidth optimal regresi kernel (Nadaraya-Watson) dengan
# Cross Validation (leave-one-out)
# ------------------------------------------------------------------------------
# cv1(X, Y, a, b, c): mencoba bandwidth h = a, a+c, ..., b
# Mengembalikan list: h.opt, tabel (h, CV), top10
# Membutuhkan fungsi ker() dari Regresi_Nonparametrik/helpers/kernel.R
# ==============================================================================

cv1 <- function(X, Y, a, b, c) {
  h <- seq(a, b, by = c)
  s <- length(h)
  n <- length(Y)
  CV <- numeric(s)
  for (m in 1:s) {
    Ghat <- numeric(n)
    for (j in 1:n) {
      bobot  <- ker((X[j] - X) / h[m])
      faktor <- sum(bobot)
      # Leave-one-out: buang kontribusi observasi ke-j sendiri
      Ghat[j] <- if ((faktor - ker(0)) == 0) 0 else
        (sum(bobot * Y) - ker(0) * Y[j]) / (faktor - ker(0))
    }
    CV[m] <- mean((Y - Ghat)^2)
  }
  R <- cbind(h = h, CV = CV)
  S <- R[order(R[, "CV"]), , drop = FALSE][1:min(10, s), , drop = FALSE]
  cat("\nh opt =", S[1, 1], "dengan CV minimal =", S[1, 2],
      "\nBerikut 10 nilai CV terkecil beserta nilai bandwidth h:\n")
  print(S)
  invisible(list(h.opt = S[1, 1], tabel = R, top10 = S))
}
