# ==============================================================================
# Regresi Logistik Biner
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : generalhoslem
# Data    : data/reglog_admisi.csv (data penerimaan mahasiswa: admit, gre, gpa, rank)
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
# ==============================================================================

data_reglog <- read.csv("data/reglog_admisi.csv")
str(data_reglog)
data_reglog$admit <- as.factor(data_reglog$admit)

model <- glm(admit ~ gre + gpa + rank, data = data_reglog, family = "binomial")
model
summary(model)

# Peluang diterima untuk tiap mahasiswa
prediksi <- predict(model, data_reglog, type = "response")
head(prediksi)

# Uji kecocokan model Hosmer-Lemeshow
library(generalhoslem)
logitgof(data_reglog$admit, fitted(model))
