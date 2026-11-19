# ==============================================================================
# Mixed Geographically Weighted Regression (MGWR)
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : spgwr
# Data    : data/mgwr.csv (data dummy 38 wilayah, lihat data/generate_dummy_data.R)
# Catatan : Buka R-Studio-Tutorial.Rproj agar working directory = root repo.
#
# Model   : Merokok = b0(u,v) + b1(u,v)*Pendidikan + b2(u,v)*Kemiskinan   (lokal)
#                     + g1*Pria                                         (global)
# ==============================================================================

library(spgwr)

dataku <- read.csv("data/mgwr.csv")
head(dataku)
coord <- cbind(dataku$U, dataku$V)

# Trace matriks (pengganti psych::tr) dan trace dari kuadrat matriks, tr(A %*% A)
tr  <- function(A) sum(diag(A))
tr2 <- function(A) sum(A * t(A))

# ------------------------------------------------------------------------------
# 1. Bandwidth adaptif (kernel Gaussian) dari model GWR penuh
# ------------------------------------------------------------------------------
adaptgauss <- gwr.sel(Merokok ~ Pendidikan + Pria + Kemiskinan, data = dataku,
                      adapt = TRUE, coords = coord, gweight = gwr.Gauss)
gwr.adaptgauss <- gwr(Merokok ~ Pendidikan + Pria + Kemiskinan, data = dataku,
                      adapt = adaptgauss, coords = coord, hatmatrix = TRUE,
                      gweight = gwr.Gauss)
gwr.adaptgauss
bw <- gwr.adaptgauss$bandwidth   # satu bandwidth untuk tiap lokasi

# ------------------------------------------------------------------------------
# 2. Variabel respon, prediktor lokal (xl), dan prediktor global (xg)
# ------------------------------------------------------------------------------
y   <- as.matrix(dataku$Merokok)
lat <- dataku$U
lon <- dataku$V
xl  <- cbind(Intercept = 1, Pendidikan = dataku$Pendidikan,
             Kemiskinan = dataku$Kemiskinan)
xg  <- cbind(Pria = dataku$Pria)
x   <- cbind(xl, xg)

ng <- ncol(xg)
nl <- ncol(xl)
n  <- length(y)
I  <- diag(1, n, n)

# ------------------------------------------------------------------------------
# 3. Matriks pembobot: baris i = bobot semua observasi untuk lokasi i.
#    Kernel Gaussian, sama dengan kernel yang dipakai saat memilih bandwidth.
# ------------------------------------------------------------------------------
d <- as.matrix(dist(cbind(lat, lon)))
W <- matrix(0, n, n)
for (i in 1:n) {
  W[i, ] <- exp(-0.5 * (d[i, ] / bw[i])^2)
}

# ------------------------------------------------------------------------------
# 4. Estimasi parameter
# ------------------------------------------------------------------------------
Sl <- matrix(0, n, n)
for (i in 1:n) {
  Wi <- diag(W[i, ])
  Sl[i, ] <- xl[i, ] %*% solve(t(xl) %*% Wi %*% xl) %*% t(xl) %*% Wi
}

ISl  <- t(I - Sl) %*% (I - Sl)
beta.g <- solve(t(xg) %*% ISl %*% xg) %*% t(xg) %*% ISl %*% y

beta.l <- matrix(0, nl, n, dimnames = list(colnames(xl), dataku$Wilayah))
for (i in 1:n) {
  Wi <- diag(W[i, ])
  beta.l[, i] <- solve(t(xl) %*% Wi %*% xl) %*% t(xl) %*% Wi %*% (y - xg %*% beta.g)
}

Sg <- xg %*% solve(t(xg) %*% xg) %*% t(xg)
S  <- Sl + (I - Sl) %*% xg %*% solve(t(xg) %*% ISl %*% xg) %*% t(xg) %*% ISl
y.hat    <- S %*% y
residual <- y - y.hat
H <- x %*% solve(t(x) %*% x) %*% t(x)   # hat matrix regresi global (OLS)

# ------------------------------------------------------------------------------
# 5. Uji serentak
#    F1: MGWR vs regresi global (OLS)
#    F2: signifikansi variabel global
#    F3: signifikansi variabel lokal
# ------------------------------------------------------------------------------
IS  <- t(I - S) %*% (I - S)
A1  <- (I - H) - IS
A2  <- t(I - Sl) %*% (I - Sl) - IS
A3  <- t(I - Sg) %*% (I - Sg) - IS
v   <- c(tr(A1), tr2(A1))
u   <- c(tr(IS), tr2(IS))
r   <- c(tr(A2), tr2(A2))
tt  <- c(tr(A3), tr2(A3))

RSS.H1 <- as.vector(t(y) %*% IS %*% y)
F1 <- as.vector((t(y) %*% A1 %*% y) / v[1]) / (RSS.H1 / u[1])
F2 <- as.vector((t(y) %*% A2 %*% y) / r[1]) / (RSS.H1 / u[1])
F3 <- as.vector((t(y) %*% A3 %*% y) / tt[1]) / (RSS.H1 / u[1])
df1 <- c(v[1]^2 / v[2], r[1]^2 / r[2], tt[1]^2 / tt[2])
df2 <- u[1]^2 / u[2]
Fgab <- c(F1, F2, F3)
p.value <- pf(Fgab, df1 = df1, df2 = df2, lower.tail = FALSE)
Uji.Serentak <- data.frame(F = Fgab, df1 = df1, df2 = df2, p.value = p.value,
                           row.names = c("F1", "F2", "F3"))

# ------------------------------------------------------------------------------
# 6. Uji parsial variabel global (uji t dua sisi)
# ------------------------------------------------------------------------------
G     <- solve(t(xg) %*% ISl %*% xg) %*% t(xg) %*% ISl
gkk   <- diag(G %*% t(G))
sigma <- sqrt(RSS.H1 / n)
t.g   <- as.vector(beta.g) / (sigma * sqrt(gkk))
p.val <- 2 * pt(-abs(t.g), df = df2)
Uji.Parsial.Global <- data.frame(beta = as.vector(beta.g), t = t.g, df = df2,
                                 p.value = p.val, row.names = colnames(xg))

# ------------------------------------------------------------------------------
# 7. Uji parsial variabel lokal di tiap lokasi (uji t dua sisi)
# ------------------------------------------------------------------------------
t.hit.l <- matrix(0, nl, n, dimnames = dimnames(beta.l))
pvalue  <- matrix(0, nl, n, dimnames = dimnames(beta.l))
for (i in 1:n) {
  Wi <- diag(W[i, ])
  M  <- solve(t(xl) %*% Wi %*% xl) %*% t(xl) %*% Wi %*% (I - xg %*% G)
  m  <- diag(M %*% t(M))
  t.hit.l[, i] <- beta.l[, i] / (sigma * sqrt(m))
  pvalue[, i]  <- 2 * pt(-abs(t.hit.l[, i]), df = df2)
}
ringkasan <- data.frame(Wilayah = dataku$Wilayah,
                        t(beta.l),
                        t = t(t.hit.l),
                        p = t(pvalue))

# ------------------------------------------------------------------------------
# 8. Selisih jumlah kuadrat residual (RSS) tiap hipotesis
# ------------------------------------------------------------------------------
RSS.H0 <- c(F1 = as.vector(t(y) %*% (I - H) %*% y),
            F2 = as.vector(t(y) %*% t(I - Sl) %*% (I - Sl) %*% y),
            F3 = as.vector(t(y) %*% t(I - Sg) %*% (I - Sg) %*% y))
DSS <- RSS.H0 - RSS.H1

# ------------------------------------------------------------------------------
# 9. Kebaikan model: AIC, AICc, R-squared, dan adjusted R-squared
# ------------------------------------------------------------------------------
AICc <- 2 * n * log(sigma) + n * log(2 * pi) + n * (n + tr(S)) / (n - 2 - tr(S))
AIC  <- 2 * n * log(sigma) + n * log(2 * pi) + n + tr(S)
RSS  <- sum(residual^2)
TSS  <- sum((y - mean(y))^2)
rsqrt <- 1 - RSS / TSS                                   # R-squared
rbar  <- 1 - (RSS / (n - ng - nl)) / (TSS / (n - 1))     # adjusted R-squared

# ------------------------------------------------------------------------------
# Hasil
# ------------------------------------------------------------------------------
beta.g
head(t(beta.l))
Uji.Serentak
Uji.Parsial.Global
head(ringkasan)
RSS.H0
RSS.H1
DSS
AICc
AIC
rsqrt
rbar
# Matriks lain (W, Sl, Sg, S, H) tersedia di Environment bila ingin diperiksa.
