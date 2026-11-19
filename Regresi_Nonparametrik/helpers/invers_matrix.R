# ==============================================================================
# Fungsi bantu: invers Moore-Penrose (generalized inverse) via SVD
# Aman dipakai untuk matriks yang singular / hampir singular.
# Dipakai oleh Regresi_Spline, Regresi_Polinomial_Lokal, dan P_Spline.
# ==============================================================================

MPL <- function(x, eps = 1e-20) {
  x <- as.matrix(x)
  xsvd <- svd(x)
  diago <- xsvd$d[xsvd$d > eps]
  if (length(diago) == 1) {
    xplus <- as.matrix(xsvd$v[, 1]) %*% t(as.matrix(xsvd$u[, 1]) / diago)
  } else {
    xplus <- xsvd$v[, 1:length(diago)] %*% diag(1 / diago) %*%
      t(xsvd$u[, 1:length(diago)])
  }
  return(xplus)
}
