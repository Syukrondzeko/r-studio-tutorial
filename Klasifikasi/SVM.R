# ==============================================================================
# Klasifikasi Support Vector Machine (SVM)
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

# SVM kernel linear
model2 <- svm(kelas ~ melahirkan + kulit, data = dataku, kernel = "linear", cost = 1)
pred2  <- predict(model2, dataku)
confusionMatrix(pred2, dataku$kelas)

# SVM kernel polinomial
SVM_poly  <- svm(kelas ~ melahirkan + kulit, data = dataku,
                 kernel = "polynomial", cost = 1, degree = 3)
pred_poly <- predict(SVM_poly, dataku)
confusionMatrix(pred_poly, dataku$kelas)

# SVM kernel radial (RBF)
SVM_rbf  <- svm(kelas ~ melahirkan + kulit, data = dataku,
                kernel = "radial", cost = 1, gamma = 0.005)
pred_rbf <- predict(SVM_rbf, dataku)
confusionMatrix(pred_rbf, dataku$kelas)
