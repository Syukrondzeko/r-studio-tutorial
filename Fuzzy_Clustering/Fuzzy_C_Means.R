# ==============================================================================
# Fuzzy C-Means Clustering
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : ppclust, factoextra
# Data    : USArrests (dataset bawaan R; sama dengan crime_data.csv di video)
# ==============================================================================

# Gunakan variabel Assault dan UrbanPop
dataku <- USArrests[, c("Assault", "UrbanPop")]
head(dataku)

library(ppclust)
set.seed(123)
c2 <- fcm(dataku, centers = 4)

# Derajat keanggotaan 6 observasi pertama
as.data.frame(c2$u)[1:6, ]
summary(c2)

# Visualisasi: ubah dulu hasil fcm ke format kmeans agar bisa dibaca factoextra
library(factoextra)
c2_km <- ppclust2(c2, "kmeans")
fviz_cluster(c2_km, data = dataku, ellipse.type = "norm", repel = TRUE,
             palette = "jco", ggtheme = theme_minimal(),
             legend = "right")
