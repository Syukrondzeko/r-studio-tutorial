# ==============================================================================
# Pemodelan SARIMA (data AirPassengers)
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : astsa
# Data    : AirPassengers (dataset bawaan R)
# ==============================================================================

AirPassengers
str(AirPassengers)
ts.plot(AirPassengers)

# Dekomposisi: tren, musiman, dan acak
components.ts <- decompose(AirPassengers)
plot(components.ts)

acf(AirPassengers)
pacf(AirPassengers)

# Transformasi log untuk menstabilkan ragam, lalu differencing
x <- log(AirPassengers)
plot(diff(x))
data <- diff(diff(x, 12))   # differencing musiman (12) dan non-musiman
plot(data)
acf(data)
pacf(data)

# SARIMA(0,1,1)(0,1,1)[12] dengan paket astsa
library(astsa)
model1 <- sarima(x, 0, 1, 1, 0, 1, 1, 12)
model1

# Peramalan 24 bulan ke depan
sarima.for(x, 24, 0, 1, 1, 0, 1, 1, 12)
