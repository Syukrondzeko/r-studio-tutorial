# ==============================================================================
# Decision Tree: CART
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : rpart, rpart.plot, caret
# Data    : ptitanic (dataset dari paket rpart.plot)
# ==============================================================================

library(rpart)
library(rpart.plot)
library(caret)
data(ptitanic)
str(ptitanic)
head(ptitanic)

# Model CART
model2 <- rpart(survived ~ pclass + sex, data = ptitanic)
model2
rpart.plot(model2)

# Prediksi dan evaluasi
pred2 <- predict(model2, type = "class")
conf.matrix2 <- table(actual = ptitanic$survived, predicted = pred2)
conf.matrix2
confusionMatrix(pred2, ptitanic$survived)
