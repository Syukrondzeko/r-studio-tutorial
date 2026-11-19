# ==============================================================================
# Fungsi bantu: fungsi truncated (x - a)^power untuk x > a, dan 0 untuk x <= a
# Dipakai oleh Regresi_Spline dan P_Spline.
# ==============================================================================

trun <- function(data, a, power) {
  data[data < a] <- a
  (data - a)^power
}
