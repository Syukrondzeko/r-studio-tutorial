# ==============================================================================
# Metode Elbow untuk Menentukan Jumlah Cluster K-Means
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : - (hanya base R)
# Data    : data/mall_customers.csv (data dummy, lihat data/generate_dummy_data.R)
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
# ==============================================================================

dataku <- read.csv("data/mall_customers.csv")
dataku <- dataku[, 3:5]
colnames(dataku) <- c("Age", "Income", "Spending")

# Within-groups sum of squares untuk k = 1 sampai 15
set.seed(123)
wss <- (nrow(dataku) - 1) * sum(apply(dataku, 2, var))   # k = 1
for (i in 2:15) {
  wss[i] <- sum(kmeans(dataku, centers = i, nstart = 10)$withinss)
}

# Jumlah cluster optimal ada di "siku" (elbow) grafik
plot(1:15, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")
