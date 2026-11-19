# ==============================================================================
# Density-Based Clustering (DBSCAN)
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : fpc, dbscan, factoextra
# Data    : iris (dataset bawaan R)
# ==============================================================================

library(fpc)
library(dbscan)
library(factoextra)

# Data iris tanpa kolom Species
data("iris")
str(iris)
newdata <- iris[, -5]

# Menentukan nilai eps optimal: cari "siku" pada kNN distance plot
kNNdistplot(newdata, k = 3)
abline(h = 0.45, lty = 2)

# DBSCAN dengan paket fpc dan paket dbscan (hasilnya sama)
set.seed(123)
f <- fpc::dbscan(newdata, eps = 0.45, MinPts = 4)
d <- dbscan::dbscan(newdata, eps = 0.45, minPts = 4)
f
d

# Visualisasi cluster
fviz_cluster(d, newdata, geom = "point")

# Keanggotaan cluster. Observasi noise/outlier diberi kode 0
f$cluster
d$cluster
table(fpc = f$cluster, dbscan = d$cluster)
