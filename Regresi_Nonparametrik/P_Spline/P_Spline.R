# ==============================================================================
# Regresi Nonparametrik Penalized Spline (P-Spline) dengan Satu Prediktor
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : - (hanya base R)
# Data    : data/pspline.csv (data dummy P2 vs IPM, lihat data/generate_dummy_data.R)
#           Di video data dibaca dari clipboard (read.table("clipboard")), yang
#           hanya berfungsi di Windows. Di sini dibaca dari file CSV.
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
# ==============================================================================

source("Regresi_Nonparametrik/helpers/invers_matrix.R")   # fungsi MPL()
source("Regresi_Nonparametrik/helpers/truncated.R")       # fungsi trun()

# Input data
data <- read.csv("data/pspline.csv")
head(data)

# Plot variabel x dan y
plot(data$P2, data$IPM, xlab = "P2", ylab = "IPM")

# Kuantil dari variabel prediktor sebagai calon titik knot
quant <- function(pred, P) {
  quantile(pred, seq(0, 1, by = 1 / P))
}

# Membentuk matriks X: kolom polinomial derajat 0..m dan k fungsi truncated
matrikx <- function(pred, m, k) {
  w  <- quant(unique(pred), k + 1)
  z1 <- outer(pred, 0:m, `^`)
  z2 <- sapply(1:k, function(j) trun(pred, w[j + 1], m))
  cbind(z1, z2)
}

# Membentuk matriks penalti D (hanya koefisien truncated yang dipenalti)
matrikd <- function(m, k) {
  d1 <- matrix(0, m + 1, m + k + 1)
  d4 <- cbind(matrix(0, k, m + 1), diag(k))
  rbind(d1, d4)
}

# Estimasi beta, nilai prediksi, matriks hat, GCV dan MSE untuk satu lambda
fit_pspline <- function(respon, pred, m, k, lam) {
  n  <- length(respon)
  y  <- as.vector(respon)
  x  <- matrikx(pred, m, k)
  d  <- matrikd(m, k)
  b2 <- MPL(t(x) %*% x + n * lam * d)
  beta  <- b2 %*% t(x) %*% y
  Hlam  <- x %*% b2 %*% t(x)
  ytopi <- x %*% beta
  MSE   <- sum((y - ytopi)^2) / n
  GCV   <- MSE / (1 - sum(diag(Hlam)) / n)^2
  list(beta = beta, ytopi = ytopi, Hlam = Hlam, GCV = GCV, MSE = MSE)
}

# Mencari lambda optimal untuk orde m dan k knot pada grid lambda bb..ba (langkah ic)
carilambda <- function(respon, pred, m, k, bb, ba, ic, plot = TRUE) {
  if (plot) {
    w <- quant(unique(pred), k + 1)
    for (a in 1:k) cat("titik knot[", a, "] =", w[a + 1], "\n")
  }
  lambda <- seq(bb, ba, ic)
  GCV <- sapply(lambda, function(l) fit_pspline(respon, pred, m, k, l)$GCV)
  if (plot) plot(lambda, GCV, type = "l",
                 main = paste("Orde", m, "-", k, "knot"))
  i_opt <- which.min(GCV)
  cbind(lambdaopt = lambda[i_opt], GCVmin = GCV[i_opt])
}

# Mencari orde, jumlah knot, dan lambda optimal.
# Jumlah knot ditambah selama GCV turun; orde ditambah selama GCV turun.
# maxorde dan maxknot membatasi pencarian agar tidak berjalan tanpa akhir.
carioptimal <- function(respon, pred, bb, ba, ic, maxorde = 4, maxknot = 10) {
  y <- as.vector(respon)
  x <- as.vector(pred)
  best <- NULL
  for (i in 1:maxorde) {
    best_k <- NULL
    for (k in 1:maxknot) {
      hasil <- carilambda(y, x, i, k, bb, ba, ic, plot = FALSE)
      baris <- c(i, k, hasil[1, "lambdaopt"], hasil[1, "GCVmin"])
      if (!is.null(best_k) && baris[4] > best_k[4]) break
      best_k <- baris
    }
    if (!is.null(best) && best_k[4] > best[4]) break
    best <- best_k
  }
  mopt <- matrix(best, nrow = 1,
                 dimnames = list(NULL, c("Orde", "Jumlah Knot", "Lambda", "GCV")))
  mopt
}

# Estimasi parameter P-Spline dengan orde, knot, dan lambda optimal
pspline.satu <- function(respon, predictor, bb, ba, ic) {
  Y <- as.vector(respon)
  X <- as.vector(predictor)
  n <- length(Y)
  optimal <- carioptimal(Y, X, bb, ba, ic)
  print(optimal)
  m <- optimal[, 1]
  k <- optimal[, 2]
  h <- optimal[, 3]

  w <- quant(unique(X), k + 1)
  cat("orde:", m, "\n")
  cat("lambda:", h, "\n")
  cat("jumlah knot:", k, "\n")
  for (i in 1:k) cat("titik knots[", i, "] =", w[i + 1], "\n")

  carilambda(Y, X, m, k, bb, ba, ic, plot = TRUE)   # plot GCV terhadap lambda
  hasil <- fit_pspline(Y, X, m, k, h)
  fstar <- hasil$ytopi
  error <- Y - fstar
  cat("\nGCV =", hasil$GCV)
  cat("\nMSE =", format(hasil$MSE), "\n")
  print(data.frame(X = X, Y = Y, Y_topi = as.vector(fstar), error = as.vector(error)))
  for (i in 1:(m + k + 1)) cat("\n nilai beta[", i, "] =", hasil$beta[i])
  cat("\n")

  urut <- order(X)
  plot(X, Y, xlab = "Prediktor", ylab = "Respon",
       main = "Fungsi Penalized Spline untuk Satu Prediktor")
  lines(X[urut], fstar[urut], col = "red", lwd = 2)
  invisible(c(list(optimal = optimal), hasil))
}

# Grid lambda: dari 0 sampai 0.05 dengan langkah 0.0005.
# (Sesuaikan rentang ini dengan data Anda.)
pspline.satu(data$IPM, data$P2, bb = 0, ba = 0.05, ic = 0.0005)
