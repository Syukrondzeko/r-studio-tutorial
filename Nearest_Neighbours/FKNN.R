# ==============================================================================
# Fuzzy K-Nearest Neighbor (FKNN)
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : - (hanya base R)
# Data    : data/knn.csv (data dummy, lihat data/generate_dummy_data.R)
#           Baris 1-17 = data latih, baris 18 = data uji.
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
# ==============================================================================

data_knn <- read.csv("data/knn.csv")
data_knn

# Pembagian data latih dan data uji
nrow(data_knn)
train <- data_knn[1:17, ]
test  <- data_knn[18, ]

# Jarak Euclidean setiap data latih ke data uji, dan bobot = 1 / jarak^2
# (pangkat 2 berasal dari 2/(m-1) dengan m = 2)
train$jarak <- sqrt((train$x1 - test$x1)^2 + (train$x2 - test$x2)^2)
train$d     <- train$jarak^(-2)

# Ambil k tetangga terdekat
k <- 3
tetangga <- train[order(train$jarak)[1:k], ]
tetangga

# Derajat keanggotaan tiap kelas = jumlah bobot tetangga di kelas tersebut
# dibagi jumlah bobot seluruh tetangga
Prob_kelas_0 <- sum(tetangga$d[tetangga$y == 0]) / sum(tetangga$d)
Prob_kelas_1 <- sum(tetangga$d[tetangga$y == 1]) / sum(tetangga$d)
cat("Probabilitas Kelas 0 adalah", Prob_kelas_0, "\n")
cat("Probabilitas Kelas 1 adalah", Prob_kelas_1, "\n")
cat("Data uji masuk ke kelas", ifelse(Prob_kelas_1 > Prob_kelas_0, 1, 0), "\n")
