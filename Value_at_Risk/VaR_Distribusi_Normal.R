# ==============================================================================
# Value at Risk (VaR) Saham Tunggal: Distribusi Normal
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : - (hanya base R)
# Data    : data/var_saham.csv (data dummy return saham TLKM, WSKT, UNVR,
#           lihat data/generate_dummy_data.R)
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
# ==============================================================================

Data_Mentah <- read.csv("data/var_saham.csv")
head(Data_Mentah)

# Ambil kolom return. Baris pertama dibuang karena return-nya NA
# (belum ada harga periode sebelumnya).
Data_VaR <- na.omit(Data_Mentah[, c("r_TLKM", "r_WSKT", "r_UNVR")])
head(Data_VaR)

data <- Data_VaR$r_TLKM
n <- length(data)

alpha <- 0.05      # tingkat kepercayaan 95%
hp    <- 1         # holding period (periode)
modal <- 1000000   # modal investasi (Rp)
m <- mean(data)
s <- sd(data)

# VaR distribusi normal (dengan asumsi rata-rata return = 0)
z <- qnorm(alpha)
VaR_Z <- -(modal * sqrt(hp) * z * s)
cat("VaR Normal", (1 - alpha) * 100, "% :", VaR_Z, "\n")
