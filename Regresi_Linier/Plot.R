# ==============================================================================
# Plot Hubungan Variabel pada Regresi Linier
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : ggplot2, gridExtra
# Catatan : Script ini menjalankan Pemodelan_Regresi.R terlebih dahulu untuk
#           mendapatkan objek `dataku`.
# ==============================================================================

source("Regresi_Linier/Pemodelan_Regresi.R")

library(ggplot2)
p1 <- ggplot(dataku, aes(x = jml_unit, y = waktu)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x)
p2 <- ggplot(dataku, aes(x = jarak, y = waktu)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x)
gridExtra::grid.arrange(p1, p2, ncol = 2)
