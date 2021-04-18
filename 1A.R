library(leaps)
library(car)
setwd('D:/Master/HK1-DS/Statistic Modeling/data')
wine <- read.csv("winequality-red.csv", header = TRUE)

###summary
head(wine)
dim(wine)
summary(wine)

###correlation
corrplot::corrplot(cor(wine), addCoef.col = "grey", number.cex = 0.7)

###full model
attach(wine)
m_wine_full <- lm(quality ~ ., data = wine)
summary(m_wine_full)

### possible subsets:
X_wine <- cbind(fixed.acidity, volatile.acidity, citric.acid, residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, density, pH, sulphates, alcohol)
b_wine <- regsubsets(as.matrix(X_wine), quality)
rs_wine <- summary(b_wine)
rs_wine$outmat
rs_wine$bic

#####selected model
m_6 <- lm(quality ~ volatile.acidity + chlorides + total.sulfur.dioxide + pH + sulphates + alcohol) 
summary(m_6)

### check VIF:
vif(m_6)

###Partial F-test for selected model:
anova(m_6, m_wine_full)

