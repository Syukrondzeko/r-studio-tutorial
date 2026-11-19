# ==============================================================================
# Hierarchical Clustering
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : - (hanya base R)
# Data    : data/mall_customers.csv (data dummy, lihat data/generate_dummy_data.R)
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
# ==============================================================================

dataku <- read.csv("data/mall_customers.csv")
dataku <- dataku[, 3:5]
colnames(dataku) <- c("Age", "Income", "Spending")
head(dataku)

# Matriks jarak Euclidean
distance <- dist(dataku)

# Complete linkage
hc.c <- hclust(distance, method = "complete")
plot(hc.c)

# Average linkage
hc.a <- hclust(distance, method = "average")
plot(hc.a, hang = -1)
rect.hclust(hc.a, k = 5, border = 2:6)

# Potong dendrogram menjadi 5 cluster dan hitung rata-rata tiap cluster
member.c <- cutree(hc.c, 5)
member.a <- cutree(hc.a, 5)
aggregate(dataku, list(cluster = member.c), mean)
aggregate(dataku, list(cluster = member.a), mean)

# Bandingkan keanggotaan cluster kedua metode
table(complete = member.c, average = member.a)
