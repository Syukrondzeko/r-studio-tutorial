# ==============================================================================
# K-Means Clustering
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : ggplot2
# Data    : data/mall_customers.csv (data dummy, lihat data/generate_dummy_data.R)
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
# ==============================================================================

# Membaca data: ambil kolom Age, Annual_Income, Spending_Score
dataku <- read.csv("data/mall_customers.csv")
dataku <- dataku[, 3:5]
colnames(dataku) <- c("Age", "Income", "Spending")
head(dataku)

# K-means dengan 5 cluster.
# set.seed agar hasil selalu sama (pusat awal k-means dipilih secara acak),
# nstart = 25 -> coba 25 pusat awal berbeda lalu ambil hasil terbaik.
set.seed(123)
fit <- kmeans(dataku, centers = 5, nstart = 25)

# Rata-rata tiap cluster
aggregate(dataku, by = list(cluster = fit$cluster), FUN = mean)

# Tambahkan label cluster ke data
dataku2 <- data.frame(dataku, fit.cluster = as.factor(fit$cluster))
head(dataku2)
str(dataku2)

# Visualisasi
library(ggplot2)
ggplot(dataku2, aes(x = Income, y = Spending, col = fit.cluster, size = Age)) +
  geom_point()
