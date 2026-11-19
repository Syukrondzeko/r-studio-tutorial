# ==============================================================================
# Pemodelan ARIMA
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : tseries
# Data    : data/arima.csv (data dummy MA(1), lihat data/generate_dummy_data.R)
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
# ==============================================================================

# Contoh membangkitkan data MA(1) sendiri:
# data_sim <- arima.sim(n = 50, model = list(order = c(0, 0, 1), ma = 0.1), sd = 0.1)

data1 <- read.csv("data/arima.csv")
head(data1)
data1 <- ts(data1$x)
str(data1)
plot.ts(data1)

# Uji stasioneritas (Augmented Dickey-Fuller)
library(tseries)
adf.test(data1)

# Identifikasi orde dari plot ACF dan PACF
acf(data1)
pacf(data1)

# Estimasi model MA(1) = ARIMA(0,0,1)
fit <- arima(data1, order = c(0, 0, 1))
fit

# Diagnostik residual
myresid <- residuals(fit)
qqnorm(myresid)
qqline(myresid)

# Uji Ljung-Box untuk banyak lag sekaligus.
# (Pengganti FitAR::LjungBoxTest, karena paket FitAR sudah dihapus dari CRAN.)
fitdf   <- 1                      # banyaknya parameter ARMA (MA(1) -> 1)
lags    <- (fitdf + 1):20
pval_lb <- sapply(lags, function(l)
  Box.test(myresid, lag = l, type = "Ljung-Box", fitdf = fitdf)$p.value)
plot(lags, pval_lb, ylim = c(0, 1), main = "Ljung-Box Test",
     xlab = "Lag", ylab = "p-value")
abline(h = 0.05, lty = 2, col = "red")

Box.test(myresid, type = "Ljung-Box")

# Peramalan 5 periode ke depan
pred <- predict(fit, n.ahead = 5)
pred
ts.plot(data1, pred$pred, col = c("black", "red"))
