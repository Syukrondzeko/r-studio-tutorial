# ==============================================================================
# Regresi Nonparametrik Polinomial Lokal
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : - (hanya base R)
# Data    : data/regresi_polinomial_lokal.csv (data dummy usia vs kolesterol,
#           lihat data/generate_dummy_data.R)
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
# ==============================================================================

source("Regresi_Nonparametrik/helpers/kernel.R")          # fungsi ker()
source("Regresi_Nonparametrik/helpers/invers_matrix.R")   # fungsi MPL()
source("Regresi_Nonparametrik/Regresi_Polinomial_Lokal/GCV_Polinomial.R")

dataku <- read.csv("data/regresi_polinomial_lokal.csv")
head(dataku)
X <- dataku$usia
Y <- dataku$kolesterol

# Estimasi polinomial lokal derajat p dengan bandwidth h.
# Menampilkan parameter lokal di titik x0, lalu kurva estimasi di semua titik.
LocalPolynomial <- function(p, x0, h, X, Y) {
  n <- length(X)
  lokal <- beta_lokal(x0, X, Y, p, h)
  H_hat <- hat_lokal(X, p, h)
  m_hat <- H_hat %*% Y
  MSE <- sum((Y - m_hat)^2) / n
  GCV <- MSE / (1 - sum(diag(H_hat)) / n)^2
  cat("=========================================", "\n")
  cat("       Estimasi Parameter Lokal          ", "\n")
  print(lokal$beta)
  cat("=========================================", "\n")
  cat("dengan nilai X0 =", x0, "-> estimasi m(X0) =", lokal$beta[1], "\n")
  cat("Derajat polinomial =", p, "\n")
  cat("Bandwidth =", h, "\n")
  cat("GCV =", GCV, "\nMSE =", MSE, "\n")
  cat("=========================================", "\n")
  cat("    Estimasi Y")
  cat("\n================\n")
  print(head(m_hat))
  cat("================\n")
  urut <- order(X)
  plot(X, Y, type = "p", xlab = "USIA", ylab = "KADAR KOLESTEROL")
  lines(X[urut], m_hat[urut], col = "red", lwd = 2)
  points(x0, lokal$beta[1], pch = 19, col = "blue")
  invisible(list(beta_x0 = lokal$beta, fitted = m_hat, GCV = GCV, MSE = MSE))
}

# 1. Cari bandwidth optimal (polinomial derajat 1 = lokal linier)
hasil_gcv <- gcv_LocalPolynomial(X, Y, band = 1, width = 20, p = 1, by = 0.5)
plot(hasil_gcv$tabel[, "h"], hasil_gcv$tabel[, "GCV"], type = "l",
     xlab = "h", ylab = "GCV")

# 2. Estimasi dengan h optimal; parameter lokal ditampilkan di X0 = median usia
LocalPolynomial(p = 1, x0 = median(X), h = hasil_gcv$h.opt, X = X, Y = Y)
