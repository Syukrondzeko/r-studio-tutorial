# ==============================================================================
# Fuzzy K-Nearest Neighbor in Every Class (FKNNC)
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : - (hanya base R)
# Data    : data/knn.csv (data dummy, lihat data/generate_dummy_data.R)
#           Baris 1-17 = data latih, baris 18 = data uji.
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
# ==============================================================================

data_knn <- read.csv("data/knn.csv")

# Pembagian data latih dan data uji
nrow(data_knn)
train <- data_knn[1:17, ]
test  <- data_knn[18, ]

# Jarak Euclidean ke data uji dan bobot = 1 / jarak^2
train$jarak <- sqrt((train$x1 - test$x1)^2 + (train$x2 - test$x2)^2)
train$d     <- train$jarak^(-2)

# Ambil k tetangga terdekat DI SETIAP KELAS, lalu jumlahkan bobotnya
k <- 2
kelas <- sort(unique(train$y))
S <- sapply(kelas, function(cl) {
  train_cl <- train[train$y == cl, ]
  terdekat <- train_cl[order(train_cl$jarak)[1:k], ]
  sum(terdekat$d)
})
names(S) <- paste("kelas", kelas)
S

# Derajat keanggotaan data uji untuk tiap kelas
Prob <- S / sum(S)
cat("Probabilitas Kelas 0 adalah", Prob[1], "\n")
cat("Probabilitas Kelas 1 adalah", Prob[2], "\n")
cat("Data uji masuk ke kelas", kelas[which.max(Prob)], "\n")
