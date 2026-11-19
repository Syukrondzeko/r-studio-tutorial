# ==============================================================================
# Generator Data Dummy
# ------------------------------------------------------------------------------
# Script ini membuat semua dataset dummy (sintetis) yang dipakai oleh tutorial.
# Data asli yang dipakai di video tidak disertakan, sehingga dataset di folder
# data/ dibuat ulang dengan struktur kolom yang sama agar semua script bisa
# langsung dijalankan.
#
# PENTING: Data ini BUKAN data nyata. Angka-angkanya dibangkitkan secara acak
# (dengan set.seed agar hasilnya selalu sama) hanya untuk keperluan belajar.
#
# Cara pakai: buka R-Studio-Tutorial.Rproj, lalu jalankan
#   source("data/generate_dummy_data.R")
# Hanya membutuhkan base R (tanpa paket tambahan).
#
# Catatan: data/reglog_admisi.csv adalah data asli dari repo ini dan tidak
# dibuat oleh script ini.
# ==============================================================================

out <- function(df, file) {
  path <- file.path("data", file)
  write.csv(df, path, row.names = FALSE)
  message("Ditulis: ", path, " (", nrow(df), " baris)")
}

# ------------------------------------------------------------------------------
# 1. ANOVA & Kruskal-Wallis: 5 perlakuan x 5 ulangan (format panjang)
# ------------------------------------------------------------------------------
set.seed(101)
perlakuan <- rep(c("A", "B", "C", "D", "E"), each = 5)
rata2     <- c(A = 60, B = 65, C = 72, D = 68, E = 75)
anova_df  <- data.frame(
  Perlakuan = perlakuan,
  Nilai     = round(rata2[perlakuan] + rnorm(25, 0, 4), 1)
)
out(anova_df, "anova.csv")

# ------------------------------------------------------------------------------
# 2. ARIMA: deret waktu MA(1) dengan 50 observasi
# ------------------------------------------------------------------------------
set.seed(102)
arima_df <- data.frame(
  x = round(as.numeric(arima.sim(n = 50, model = list(ma = 0.6), sd = 1)) + 10, 4)
)
out(arima_df, "arima.csv")

# ------------------------------------------------------------------------------
# 3. Auto ARIMA: harga penutupan harian dua saham (random walk + drift)
# ------------------------------------------------------------------------------
set.seed(103)
n_hari  <- 120
tanggal <- seq(as.Date("2020-01-01"), by = "day", length.out = n_hari * 2)
tanggal <- tanggal[!weekdays(tanggal) %in% c("Saturday", "Sunday", "Sabtu", "Minggu")][1:n_hari]
auto_arima_df <- data.frame(
  Tanggal = tanggal,
  WSKT    = round(1500 + cumsum(rnorm(n_hari, -2, 25))),
  TLKM    = round(3500 + cumsum(rnorm(n_hari,  1, 40)))
)
out(auto_arima_df, "auto_arima.csv")

# ------------------------------------------------------------------------------
# 4. Clustering & Regresi: data pelanggan mall (200 pelanggan, 5 segmen)
# ------------------------------------------------------------------------------
set.seed(104)
segmen <- data.frame(
  income = c(25, 25, 55, 85, 85),  # ribu dolar per tahun
  spend  = c(20, 80, 50, 15, 85),  # skor belanja 1-100
  age    = c(45, 25, 42, 40, 32),
  n      = c(23, 22, 80, 37, 38)
)
mall_list <- lapply(seq_len(nrow(segmen)), function(i) {
  s <- segmen[i, ]
  data.frame(
    Age            = round(pmin(pmax(rnorm(s$n, s$age, 8), 18), 70)),
    Annual_Income  = round(pmin(pmax(rnorm(s$n, s$income, 7), 15), 140)),
    Spending_Score = round(pmin(pmax(rnorm(s$n, s$spend, 8), 1), 100))
  )
})
mall <- do.call(rbind, mall_list)
mall <- mall[sample(nrow(mall)), ]
mall_df <- data.frame(
  CustomerID = seq_len(nrow(mall)),
  Gender     = sample(c("Male", "Female"), nrow(mall), replace = TRUE),
  mall,
  row.names  = NULL
)
out(mall_df, "mall_customers.csv")

# ------------------------------------------------------------------------------
# 5. Naive Bayes & SVM: klasifikasi kelas hewan
# ------------------------------------------------------------------------------
hewan_df <- data.frame(
  hewan = c("kucing", "anjing", "sapi", "kambing", "kuda", "paus", "kelinci",
            "ayam", "bebek", "elang", "merpati", "burung hantu", "angsa",
            "ular", "kadal", "buaya", "penyu", "cicak",
            "lele", "hiu martil", "nila", "kakap", "guppy"),
  penutup_kulit = c("rambut", "rambut", "rambut", "rambut", "rambut", "kulit", "rambut",
                    "bulu", "bulu", "bulu", "bulu", "bulu", "bulu",
                    "sisik", "sisik", "sisik", "sisik", "sisik",
                    "kulit", "kulit", "sisik", "sisik", "sisik"),
  melahirkan = c("ya", "ya", "ya", "ya", "ya", "ya", "ya",
                 "tidak", "tidak", "tidak", "tidak", "tidak", "tidak",
                 "tidak", "tidak", "tidak", "tidak", "tidak",
                 "tidak", "ya", "tidak", "tidak", "ya"),
  kelas = c(rep("mamalia", 7), rep("burung", 6), rep("reptil", 5), rep("ikan", 5))
)
out(hewan_df, "klasifikasi_hewan.csv")

# ------------------------------------------------------------------------------
# 6. Fuzzy K-NN: 17 data latih + 1 data uji (baris terakhir, kelas belum diketahui)
# ------------------------------------------------------------------------------
set.seed(106)
knn_df <- data.frame(
  x1 = round(c(rnorm(9, 3, 1), rnorm(8, 6, 1), 4.6), 2),
  x2 = round(c(rnorm(9, 3, 1), rnorm(8, 6, 1), 4.4), 2),
  y  = c(rep(0, 9), rep(1, 8), NA)
)
out(knn_df, "knn.csv")

# ------------------------------------------------------------------------------
# 7. Mixed GWR: 38 wilayah dengan koordinat (lintang U, bujur V)
#    Pendidikan & Kemiskinan berpengaruh lokal, Pria berpengaruh global.
# ------------------------------------------------------------------------------
set.seed(107)
n_wil <- 38
U <- round(runif(n_wil, -8.4, -6.9), 4)    # lintang
V <- round(runif(n_wil, 111.0, 114.4), 4)  # bujur
Pendidikan <- round(rnorm(n_wil, 8, 1.2), 2)    # rata-rata lama sekolah
Pria       <- round(rnorm(n_wil, 49.5, 1), 2)   # persentase penduduk pria
Kemiskinan <- round(rnorm(n_wil, 11, 3), 2)     # persentase penduduk miskin
b_pend  <- -1.5 + 0.8 * (V - mean(V))           # efek lokal berubah dari barat ke timur
b_miskin <- 0.6 + 0.5 * (U - mean(U))           # efek lokal berubah dari utara ke selatan
Merokok <- round(10 + b_pend * Pendidikan + 0.4 * Pria + b_miskin * Kemiskinan +
                   rnorm(n_wil, 0, 0.8), 2)
mgwr_df <- data.frame(
  Wilayah = sprintf("Wilayah_%02d", seq_len(n_wil)),
  Merokok, Pendidikan, Pria, Kemiskinan, U, V
)
out(mgwr_df, "mgwr.csv")

# ------------------------------------------------------------------------------
# 8. Regresi Kernel: kurs rupiah vs indeks LQ45 (hubungan nonlinier)
# ------------------------------------------------------------------------------
set.seed(108)
kurs <- round(runif(60, 13300, 15200))
lq45 <- round(1000 - 0.00012 * (kurs - 13800)^2 + 40 * sin((kurs - 13300) / 250) +
                rnorm(60, 0, 12), 2)
out(data.frame(kurs, lq45), "regresi_kernel.csv")

# ------------------------------------------------------------------------------
# 9. Regresi Spline: usia pasien vs kadar gula darah (patahan di usia ~54)
# ------------------------------------------------------------------------------
set.seed(109)
usia <- sort(round(runif(50, 30, 75)))
gula_darah <- round(185 + 0.15 * (usia - 30) + 1.1 * pmax(usia - 54, 0) +
                      rnorm(50, 0, 2.5), 1)
out(data.frame(usia, gula_darah), "regresi_spline.csv")

# ------------------------------------------------------------------------------
# 10. Regresi Polinomial Lokal: usia vs kadar kolesterol
# ------------------------------------------------------------------------------
set.seed(110)
usia2 <- sort(round(runif(40, 20, 70)))
kolesterol <- round(150 + 2.2 * (usia2 - 20) - 0.025 * (usia2 - 20)^2 +
                      8 * sin(usia2 / 6) + rnorm(40, 0, 5), 1)
out(data.frame(usia = usia2, kolesterol), "regresi_polinomial_lokal.csv")

# ------------------------------------------------------------------------------
# 11. P-Spline: indeks keparahan kemiskinan (P2) vs IPM, 38 wilayah
# ------------------------------------------------------------------------------
set.seed(111)
P2  <- round(runif(38, 0.1, 1.6), 3)
IPM <- round(78 - 9 * P2 + 3.5 * pmax(P2 - 0.8, 0) + 1.5 * sin(4 * P2) +
               rnorm(38, 0, 0.9), 2)
out(data.frame(P2, IPM), "pspline.csv")

# ------------------------------------------------------------------------------
# 12. Value at Risk: harga & return mingguan 3 saham (40 periode)
#     Baris pertama return-nya NA karena belum ada harga sebelumnya.
# ------------------------------------------------------------------------------
set.seed(112)
n_minggu <- 40
harga <- function(p0, mu, sigma) round(p0 * exp(cumsum(c(0, rnorm(n_minggu - 1, mu, sigma)))))
log_return <- function(p) c(NA, round(diff(log(p)), 6))
TLKM <- harga(3800, 0.002, 0.030)
WSKT <- harga(1600, -0.004, 0.060)
UNVR <- harga(7500, 0.001, 0.025)
var_df <- data.frame(
  Tanggal = seq(as.Date("2020-01-06"), by = "week", length.out = n_minggu),
  TLKM, r_TLKM = log_return(TLKM),
  WSKT, r_WSKT = log_return(WSKT),
  UNVR, r_UNVR = log_return(UNVR)
)
out(var_df, "var_saham.csv")
