# ==============================================================================
# Geographically Weighted Regression (GWR) dengan Bandwidth Tetap (Fixed)
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : spgwr, spData, sf (opsional, untuk peta)
# Data    : columbus (dataset dari paket spData). Di video data ini dibaca dari
#           columbus.txt; kolom crime/income/housing = CRIME/INC/HOVAL.
# ==============================================================================

library(spgwr)
data(columbus, package = "spData")
head(columbus)
str(columbus)
# Dokumentasi data: ?spData::columbus

# Regresi global (OLS)
a <- lm(CRIME ~ INC + HOVAL, data = columbus)
summary(a)

koordinat <- cbind(columbus$X, columbus$Y)

# Pemilihan bandwidth tetap optimal (kernel Gaussian)
h <- gwr.sel(CRIME ~ INC + HOVAL, coords = koordinat,
             data = columbus, adapt = FALSE, gweight = gwr.Gauss)

gwr2 <- gwr(CRIME ~ INC + HOVAL, coords = koordinat, bandwidth = h,
            data = columbus, hatmatrix = TRUE, gweight = gwr.Gauss)
gwr2
names(gwr2)
names(gwr2$SDF)

# Menampilkan nilai koefisien beta
gwr2$SDF$"(Intercept)"
gwr2$SDF$INC
gwr2$SDF$HOVAL

# Uji kecocokan model
BFC02.gwr.test(gwr2)

# Uji pengaruh geografis terhadap setiap prediktor
LMZ.F3GWR.test(gwr2)

# Melihat nilai bandwidth
gwr2$bandwidth

# Menampilkan nilai koefisien, nilai prediksi, dan R2 lokal
hasil <- gwr2$SDF@data   # tabel atribut (nama kolom tetap asli)
head(hasil[, c("(Intercept)", "INC", "HOVAL", "pred", "localR2")])

# Peta koefisien lokal INC (membutuhkan paket sf).
# Catatan: paket rgdal (readOGR) sudah dihapus dari CRAN, diganti sf::st_read.
if (requireNamespace("sf", quietly = TRUE)) {
  peta <- sf::st_read(system.file("shapes/columbus.gpkg", package = "spData"),
                      quiet = TRUE)
  peta$beta_INC <- hasil$INC
  plot(peta["beta_INC"], main = "Koefisien lokal INC")
}
