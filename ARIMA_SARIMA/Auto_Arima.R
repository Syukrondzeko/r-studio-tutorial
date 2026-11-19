# ==============================================================================
# Auto ARIMA (pemilihan orde ARIMA otomatis)
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : forecast
# Data    : data/auto_arima.csv (data dummy harga saham, lihat
#           data/generate_dummy_data.R)
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
#           Dokumentasi fungsi: ?forecast::auto.arima
# ==============================================================================

library(forecast)

Data <- read.csv("data/auto_arima.csv")
head(Data)
tsdata <- ts(Data$WSKT)
str(tsdata)
plot(tsdata)

model <- auto.arima(tsdata)
model

# Peramalan 7 periode ke depan
prediksi <- forecast(model, h = 7)
prediksi

# Plot hasil prediksi
plot(prediksi)

# Cek residual
plot(prediksi$residuals)
qqnorm(prediksi$residuals)
qqline(prediksi$residuals)
checkresiduals(model)

# Cek akurasi
summary(model)
accuracy(model)
