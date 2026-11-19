# ==============================================================================
# Uji Asumsi Klasik Regresi Linier
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : lmtest, car
# Catatan : Script ini menjalankan Pemodelan_Regresi.R terlebih dahulu untuk
#           mendapatkan objek `model`.
# ==============================================================================

source("Regresi_Linier/Pemodelan_Regresi.R")
residu <- residuals(model)

# Uji asumsi normalitas
qqnorm(residu)
qqline(residu)
ks.test(residu, "pnorm", 0, sd(residu))
shapiro.test(residu)

# Uji homoskedastisitas (Breusch-Pagan)
library(lmtest)
bptest(model)

# Uji asumsi non-autokorelasi (Durbin-Watson)
dwtest(model)

# Uji asumsi non-multikolinieritas (VIF)
library(car)
vif(model)
