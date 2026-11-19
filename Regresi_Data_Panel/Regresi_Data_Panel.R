# ==============================================================================
# Regresi Data Panel (Common, Fixed, dan Random Effect)
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : plm  (install sekali saja: install.packages("plm"))
# Data    : Grunfeld (dataset dari paket plm)
# ==============================================================================

library(plm)

data("Grunfeld", package = "plm")
head(Grunfeld)

# Common effect (pooled OLS), fixed effect (within), dan random effect
grun.ce <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
grun.fe <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
grun.re <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
summary(grun.fe)
fixef(grun.fe)
summary(grun.re)
ranef(grun.re)

# Uji Chow (F test): fixed effect vs common effect
pFtest(grun.fe, grun.ce)

# Uji Hausman: fixed effect vs random effect
phtest(grun.fe, grun.re)

# Uji Lagrange Multiplier (Breusch-Pagan): random effect vs common effect
plmtest(grun.ce, type = "bp")
