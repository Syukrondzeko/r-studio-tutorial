# ==============================================================================
# ANOVA Satu Arah dari Nol (tanpa paket)
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : - (hanya base R)
# Data    : data/anova.csv (data dummy, lihat data/generate_dummy_data.R)
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
# ==============================================================================

data_anova <- read.csv("data/anova.csv")
data_anova$Perlakuan <- as.factor(data_anova$Perlakuan)
head(data_anova)

# Pisahkan nilai per perlakuan -> list berisi satu vektor untuk tiap kelompok
kelompok <- split(data_anova$Nilai, data_anova$Perlakuan)
kelompok

# Fungsi ANOVA manual. Berlaku untuk berapa pun jumlah perlakuan dan ulangan
# (derajat bebas dihitung dari data, bukan ditulis manual).
anova_manual <- function(kelompok, alpha = 0.05) {
  k   <- length(kelompok)             # banyaknya perlakuan
  ni  <- sapply(kelompok, length)     # banyaknya ulangan tiap perlakuan
  N   <- sum(ni)                      # total observasi
  Ti  <- sapply(kelompok, sum)        # total tiap perlakuan
  G   <- sum(Ti)                      # total keseluruhan

  JKP <- sum(Ti^2 / ni) - G^2 / N             # jumlah kuadrat perlakuan
  JKT <- sum(unlist(kelompok)^2) - G^2 / N    # jumlah kuadrat total
  JKE <- JKT - JKP                            # jumlah kuadrat error

  dk1 <- k - 1                                # db perlakuan
  dk2 <- N - k                                # db error
  Fhit   <- (JKP / dk1) / (JKE / dk2)
  Ftabel <- qf(1 - alpha, df1 = dk1, df2 = dk2)
  pvalue <- pf(Fhit, dk1, dk2, lower.tail = FALSE)

  cat("JKP      :", JKP, "\n")
  cat("JKE      :", JKE, "\n")
  cat("JKT      :", JKT, "\n")
  cat("db       :", dk1, "dan", dk2, "\n")
  cat("F Hitung :", Fhit, "\n")
  cat("F Tabel  :", Ftabel, " (alpha =", alpha, ")\n")
  cat("p-value  :", pvalue, "\n")
  if (Fhit > Ftabel) {
    cat("Keputusan: Tolak H0 -> minimal ada satu perlakuan yang berbeda\n")
  } else {
    cat("Keputusan: Gagal tolak H0 -> tidak ada perbedaan antar perlakuan\n")
  }
  invisible(list(JKP = JKP, JKE = JKE, JKT = JKT, df = c(dk1, dk2),
                 F = Fhit, F_tabel = Ftabel, p_value = pvalue))
}

# Di video digunakan taraf signifikansi 1% (alpha = 0.01)
anova_manual(kelompok, alpha = 0.01)

# Cek: hasilnya harus sama dengan fungsi bawaan R
summary(aov(Nilai ~ Perlakuan, data = data_anova))
