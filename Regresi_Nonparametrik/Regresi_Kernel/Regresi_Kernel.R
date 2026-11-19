# ==============================================================================
# Regresi Nonparametrik Kernel (Estimator Nadaraya-Watson)
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : - (hanya base R)
# Data    : data/regresi_kernel.csv (data dummy kurs vs LQ45,
#           lihat data/generate_dummy_data.R)
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
# ==============================================================================

source("Regresi_Nonparametrik/helpers/kernel.R")        # fungsi ker()
source("Regresi_Nonparametrik/Regresi_Kernel/GCV.R")    # fungsi gcv()
source("Regresi_Nonparametrik/Regresi_Kernel/CV.R")     # fungsi cv1()

dataku <- read.csv("data/regresi_kernel.csv")
dataku <- dataku[order(dataku$kurs), ]
head(dataku)
X <- dataku$kurs
Y <- dataku$lq45

# Estimasi Nadaraya-Watson dengan bandwidth hopt, lalu plot hasilnya
gh1 <- function(hopt, X, Y) {
  n <- length(Y)
  Ghat <- numeric(n)
  for (j in 1:n) {
    bobot  <- ker((X[j] - X) / hopt)
    faktor <- sum(bobot)
    Ghat[j] <- if (faktor == 0) 0 else sum(bobot * Y) / faktor
  }
  plot(X, Y, xlab = "Kurs Rupiah", ylab = "LQ45")
  lines(X, Ghat, col = 2, lwd = 2)
  matrix(Ghat, ncol = 1)
}

# 1. Cari bandwidth optimal dengan GCV pada rentang yang lebar
hop <- gcv(X, Y, a = 10, b = 500, c = 10)
plot(hop$tabel[, "h"], hop$tabel[, "GCV"], xlab = "h", ylab = "GCV", type = "l")

# 2. Perhalus pencarian di sekitar h optimal
hop2 <- gcv(X, Y, a = max(1, hop$h.opt - 20), b = hop$h.opt + 20, c = 1)
plot(hop2$tabel[, "h"], hop2$tabel[, "GCV"], xlab = "h", ylab = "GCV", type = "l")

# Pembanding: bandwidth optimal menurut Cross Validation
hop_cv <- cv1(X, Y, a = max(1, hop$h.opt - 20), b = hop$h.opt + 20, c = 1)

# 3. Estimasi dengan bandwidth optimal
hasil <- gh1(hop2$h.opt, X, Y)
head(hasil)
