# ==============================================================================
# Uji Kruskal-Wallis (alternatif nonparametrik untuk ANOVA satu arah)
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : - (hanya base R)
# Data    : data/anova.csv (data dummy, lihat data/generate_dummy_data.R)
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
# ==============================================================================

data_anova <- read.csv("data/anova.csv")
data_anova$Perlakuan <- as.factor(data_anova$Perlakuan)
head(data_anova)

# Kruskal-Wallis
kruskal.test(Nilai ~ Perlakuan, data = data_anova)
