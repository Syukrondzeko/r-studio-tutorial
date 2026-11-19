# ==============================================================================
# Regresi Nonparametrik Spline Truncated
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : - (hanya base R)
# Data    : data/regresi_spline.csv (data dummy usia vs kadar gula darah,
#           lihat data/generate_dummy_data.R)
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
# ==============================================================================

source("Regresi_Nonparametrik/helpers/invers_matrix.R")   # fungsi MPL()
source("Regresi_Nonparametrik/helpers/truncated.R")       # fungsi trun()
source("Regresi_Nonparametrik/Regresi_Spline/GCV.R")      # fungsi gcv1()

dataku <- read.csv("data/regresi_spline.csv")
head(dataku)
x <- dataku$usia
y <- dataku$gula_darah

# Estimasi spline orde m dengan titik knot `knots`
model.spline <- function(prediktor, respon, m, knots) {
  y <- respon
  n <- length(y)
  k <- length(knots)
  w <- matrix(0, ncol = m + k, nrow = n)
  for (i in 1:m) w[, i] <- prediktor^(i - 1)
  for (i in (m + 1):(m + k)) w[, i] <- trun(prediktor, knots[i - m], m - 1)
  wtw   <- t(w) %*% w
  Z     <- MPL(wtw)
  beta  <- Z %*% t(w) %*% y
  yfits <- w %*% beta
  MSE   <- sum((y - yfits)^2) / n
  h     <- w %*% Z %*% t(w)
  GCV   <- (n^2 * MSE) / (sum(diag(diag(n) - h)))^2

  cat("\n Spline orde", m)
  cat("\n Titik Knots  = c(", format(knots), ")")
  cat("\n Nilai GCV    = ", format(GCV),
      "\n Nilai MSE    = ", format(MSE), "\n")
  cat("\n ******************************************************************")
  cat("\n      Koefisien        Estimasi")
  cat("\n ******************************************************************")
  for (i in 1:(m + k)) cat("\n     beta[", i - 1, "]          ", format(beta[i]))
  cat("\n ******************************************************************\n")

  # Plot data dan kurva spline (diurutkan berdasarkan prediktor)
  urut <- order(prediktor)
  plot(prediktor, respon, type = "p",
       xlab = "Usia pasien (tahun)", ylab = "Kadar gula darah")
  lines(prediktor[urut], yfits[urut], col = "red", lwd = 2)

  invisible(list(beta = beta, fitted = yfits, GCV = GCV, MSE = MSE))
}

# 1. Cari titik knot optimal untuk spline linier (orde 2)
hasil_gcv <- gcv1(y, x, m = 2, l = 1)
hasil_gcv

# 2. Estimasi model dengan knot optimal
knot_opt <- hasil_gcv[1, "knot"]
model.spline(x, y, m = 2, knots = knot_opt)
