# ==============================================================================
# ANOVA: Uji Asumsi dan Uji Lanjut (Tukey HSD)
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : dplyr, car
# Data    : data/anova.csv (data dummy, lihat data/generate_dummy_data.R)
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
#           Script ini menjalankan Anova_with_libraries.R terlebih dahulu
#           untuk mendapatkan objek `data_anova` dan `res.aov`.
# ==============================================================================

source("Anova_Kruskal_Wallis/Anova_with_libraries.R")
summary(res.aov)

# Uji lanjut: Tukey HSD
TukeyHSD(res.aov)

# Uji homogenitas ragam
plot(res.aov, 1)
library(car)
leveneTest(Nilai ~ Perlakuan, data = data_anova)

# Uji normalitas residual
plot(res.aov, 2)
shapiro.test(residuals(res.aov))
