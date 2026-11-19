# ==============================================================================
# Pemodelan Regresi Linier Berganda
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : - (hanya base R)
# Data    : ditulis langsung di script (waktu pengiriman, jumlah unit, jarak)
# ==============================================================================

# Input data
waktu    <- c(9.95, 24.45, 31.75, 35.00, 25.05, 16.86, 14.38, 9.60, 24.35, 27.50,
              17.08, 37.00, 41.95, 11.66, 21.65, 17.89, 69.00, 10.30, 34.93, 46.59,
              44.88, 54.12, 56.63, 22.13, 21.15)
jml_unit <- c(2, 8, 11, 10, 8, 4, 2, 2, 9, 8, 4, 11, 12, 2, 4, 4, 20, 1, 10, 15,
              15, 16, 17, 6, 5)
jarak    <- c(50, 110, 120, 550, 295, 200, 375, 52, 100, 300, 412, 400, 500, 360,
              205, 400, 600, 585, 540, 250, 290, 510, 590, 100, 400)
dataku <- data.frame(waktu, jml_unit, jarak)
head(dataku)
str(dataku)

# Model dengan intersep
model <- lm(waktu ~ jml_unit + jarak, data = dataku)
summary(model)

# Model tanpa intersep
model2 <- lm(waktu ~ -1 + jml_unit + jarak, data = dataku)
summary(model2)
