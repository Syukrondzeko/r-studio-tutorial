# ==============================================================================
# Gustafson-Kessel Clustering
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : ppclust, factoextra
# Data    : USArrests (dataset bawaan R; sama dengan crime_data.csv di video)
#
# Catatan : Video menggunakan advclust::fuzzy.GK, tetapi paket advclust sudah
#           dihapus dari CRAN. Di sini digunakan ppclust::gk yang menjalankan
#           algoritma Gustafson-Kessel yang sama.
# ==============================================================================

dataku <- USArrests[, c("Assault", "UrbanPop")]
head(dataku)

library(ppclust)
# Lihat dokumentasi: ?ppclust::gk
set.seed(123)
c1 <- gk(dataku, centers = 4, m = 2)
summary(c1)

# Derajat keanggotaan dan cluster akhir
head(c1$u)
table(c1$cluster)

# Visualisasi
library(factoextra)
c1_km <- ppclust2(c1, "kmeans")
fviz_cluster(c1_km, data = dataku, ellipse.type = "norm", repel = TRUE,
             palette = "jco", ggtheme = theme_minimal(),
             legend = "right")
