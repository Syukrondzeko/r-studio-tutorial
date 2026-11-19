# ==============================================================================
# ANOVA Satu Arah dengan Fungsi Bawaan R
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : dplyr
# Data    : data/anova.csv (data dummy, lihat data/generate_dummy_data.R)
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
# ==============================================================================

library(dplyr)

data_anova <- read.csv("data/anova.csv")
str(data_anova)
data_anova$Perlakuan <- as.factor(data_anova$Perlakuan)
levels(data_anova$Perlakuan)

# Statistik deskriptif per perlakuan
data_anova %>%
  group_by(Perlakuan) %>%
  summarise(
    count = n(),
    mean  = mean(Nilai, na.rm = TRUE),
    sd    = sd(Nilai, na.rm = TRUE)
  )

# ANOVA satu arah
res.aov <- aov(Nilai ~ Perlakuan, data = data_anova)
summary(res.aov)
