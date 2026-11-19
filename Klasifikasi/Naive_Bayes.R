# ==============================================================================
# Klasifikasi Naive Bayes
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : e1071, caret
# Data    : data/klasifikasi_hewan.csv (data dummy, lihat data/generate_dummy_data.R)
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
# ==============================================================================

library(e1071)
library(caret)

dataku <- read.csv("data/klasifikasi_hewan.csv")
str(dataku)
colnames(dataku) <- c("hewan", "kulit", "melahirkan", "kelas")
dataku$kulit      <- as.factor(dataku$kulit)
dataku$melahirkan <- as.factor(dataku$melahirkan)
dataku$kelas      <- as.factor(dataku$kelas)
dataku <- dataku[, 2:4]
head(dataku)

# Model Naive Bayes
model1 <- naiveBayes(kelas ~ melahirkan + kulit, data = dataku)
model1

# Prediksi dan evaluasi
pred1 <- predict(model1, dataku)
pred1
confusionMatrix(pred1, dataku$kelas)
