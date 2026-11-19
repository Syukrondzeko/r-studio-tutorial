# ==============================================================================
# Regresi B-Spline
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : splines (bawaan R), splines2
# Data    : women (dataset bawaan R)
# ==============================================================================

library(splines)
library(splines2)
women

# Basis B-spline derajat 2 dengan satu knot di tinggi = 63
knot <- 63
splines::bs(women$height, knots = knot, degree = 2)
splines2::bSpline(women$height, knots = knot, degree = 2)

# Model regresi B-spline. Intersep model dipertahankan karena basis B-spline
# di atas tidak memuat kolom intersep (intercept = FALSE). Menghapus intersep
# (-1) akan memaksa kurva melewati titik nol di batas kiri.
fm1 <- lm(weight ~ splines::bs(height, knots = knot, degree = 2), data = women)
fm2 <- lm(weight ~ splines2::bSpline(height, knots = knot, degree = 2), data = women)
summary(fm1)
summary(fm2)

# Kedua paket menghasilkan nilai prediksi yang sama
all.equal(fitted(fm1), fitted(fm2))

plot(weight ~ height, data = women, pch = 19)
lines(women$height, fitted(fm1), col = "red", lwd = 2)
abline(v = knot, lty = 2)
