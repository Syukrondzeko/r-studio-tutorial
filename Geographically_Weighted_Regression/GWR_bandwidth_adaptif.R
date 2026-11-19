# ==============================================================================
# Geographically Weighted Regression (GWR) dengan Bandwidth Adaptif
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : spgwr, spData
# Data    : columbus (dataset dari paket spData)
# ==============================================================================

library(spgwr)                       # Mengaktifkan paket spgwr
data(columbus, package = "spData")   # Data columbus (sekarang ada di paket spData)
head(columbus)

# Regresi global (OLS) sebagai pembanding
a <- lm(CRIME ~ INC + HOVAL, data = columbus)
summary(a)

# Koordinat tiap wilayah
koordinat <- cbind(columbus$X, columbus$Y)

# Pemilihan bandwidth adaptif optimal (kernel Gaussian, kriteria CV)
b <- gwr.sel(CRIME ~ INC + HOVAL, coords = koordinat,
             data = columbus, adapt = TRUE, gweight = gwr.Gauss)

# Estimasi parameter
gwr1 <- gwr(CRIME ~ INC + HOVAL, coords = koordinat,
            data = columbus, adapt = b, hatmatrix = TRUE, gweight = gwr.Gauss)
gwr1
names(gwr1)
names(gwr1$SDF)

# Menampilkan nilai koefisien beta di tiap lokasi
gwr1$SDF$"(Intercept)"
gwr1$SDF$INC
gwr1$SDF$HOVAL

# Uji kecocokan model (GWR vs OLS)
BFC02.gwr.test(gwr1)

# Uji pengaruh geografis terhadap setiap prediktor
LMZ.F3GWR.test(gwr1)

# Melihat nilai bandwidth tiap lokasi
gwr1$bandwidth

# Menampilkan nilai koefisien, nilai prediksi, dan R2 lokal
hasil <- gwr1$SDF@data   # tabel atribut (nama kolom tetap asli)
head(hasil[, c("(Intercept)", "INC", "HOVAL", "pred", "localR2")])
