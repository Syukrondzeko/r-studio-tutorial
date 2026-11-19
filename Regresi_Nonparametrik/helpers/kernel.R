# ==============================================================================
# Fungsi bantu: kernel Gaussian
# Dipakai oleh Regresi_Kernel dan Regresi_Polinomial_Lokal.
# ==============================================================================

ker <- function(x) {
  (1 / sqrt(2 * pi)) * exp((-1 / 2) * x^2)
}
