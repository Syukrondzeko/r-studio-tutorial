# ==============================================================================
# Install semua paket yang dibutuhkan tutorial (cukup dijalankan sekali)
# ------------------------------------------------------------------------------
# Jalankan dari root repo:  source("install_packages.R")
# ==============================================================================

paket_cran <- c(
  # Statistik dasar & regresi
  "dplyr", "car", "lmtest", "plm", "generalhoslem", "splines2",
  # Deret waktu
  "tseries", "forecast", "astsa",
  # Clustering & klasifikasi
  "fpc", "dbscan", "factoextra", "ppclust", "e1071", "caret",
  "rpart", "rpart.plot", "partykit",
  # Spasial (GWR)
  "spgwr", "spData", "sf",
  # Market basket analysis
  "arules", "arulesViz",
  # Visualisasi
  "ggplot2", "gridExtra",
  # Shiny
  "shiny", "shinythemes", "shinydashboard"
)

belum <- setdiff(paket_cran, rownames(installed.packages()))
if (length(belum) > 0) {
  message("Menginstall: ", paste(belum, collapse = ", "))
  install.packages(belum)
}

# arules versi terbaru membutuhkan R >= 4.5. Untuk R yang lebih lama, install
# versi arsip terakhir yang masih mendukung (butuh compiler: Rtools di Windows,
# Xcode Command Line Tools di macOS).
if (!requireNamespace("arules", quietly = TRUE) && getRversion() < "4.5.0") {
  install.packages(
    "https://cloud.r-project.org/src/contrib/Archive/arules/arules_1.7-11.tar.gz",
    repos = NULL, type = "source"
  )
  install.packages("arulesViz")
}

# CHAID tidak ada di CRAN, hanya di R-Forge
if (!requireNamespace("CHAID", quietly = TRUE)) {
  install.packages("CHAID", repos = "http://R-Forge.R-project.org", type = "source")
}

semua <- c(paket_cran, "CHAID")
gagal <- semua[!vapply(semua, requireNamespace, logical(1), quietly = TRUE)]
if (length(gagal) > 0) {
  warning("Paket berikut gagal diinstall: ", paste(gagal, collapse = ", "))
} else {
  message("Semua paket siap digunakan.")
}
