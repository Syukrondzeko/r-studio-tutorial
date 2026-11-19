# ==============================================================================
# Decision Tree: CHAID
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : CHAID (dari R-Forge, bukan CRAN), partykit, rpart.plot, caret
# Data    : ptitanic (dataset dari paket rpart.plot)
#
# Paket CHAID tidak tersedia di CRAN. Install sekali saja dengan:
#   install.packages("partykit")
#   install.packages("CHAID", repos = "http://R-Forge.R-project.org", type = "source")
# ==============================================================================

library(rpart.plot)
library(caret)
library(CHAID)
data(ptitanic)
str(ptitanic)
head(ptitanic)

# CHAID hanya menerima prediktor bertipe faktor (pclass dan sex sudah faktor)
model1 <- chaid(survived ~ pclass + sex, data = ptitanic)
print(model1)
plot(model1)

# Prediksi dan evaluasi
pred1 <- predict(model1)
conf.matrix1 <- table(actual = ptitanic$survived, predicted = pred1)
conf.matrix1
confusionMatrix(pred1, ptitanic$survived)
