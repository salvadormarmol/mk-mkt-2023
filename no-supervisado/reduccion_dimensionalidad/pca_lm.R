

library(dplyr)
library(clusterSim)
library("FactoMineR")
library("factoextra")
library(pls)

crimeData <- mogavs::crimeData

crimeData %>% dim

crimeData.test <- crimeData[1451:1994,]
crimeData.descr <- crimeData[1:1450,]

crimeData.y <- crimeData.descr$y
crimeData.descr$y <- NULL

res <- cor(crimeData.descr, method="pearson")
corrplot::corrplot(res, method= "color", order = "hclust", tl.pos = 'n')

crimeData.norm <- data.Normalization(crimeData.descr, type="n1", normalization="column")
crimeData.y.norm <- data.Normalization(crimeData.y, type="n1", normalization="column")

crimeData.pca1 <- prcomp(crimeData.norm, center=TRUE, scale.=TRUE)

crimeData.pca1$x [,1:10] %>% head(1) 

as.matrix(crimeData.norm) %*% as.matrix(crimeData.pca1$rotation) [,1:10] %>% head(1)

res1 <- cor(crimeData.pca1$x, method="pearson")
corrplot::corrplot(res1, method= "color", order = "hclust", tl.pos = 'n')

plot(summary(crimeData.pca1)$importance[3,])

fviz_pca_var(crimeData.pca1,axes = c(1, 2))

pcs <- as.data.frame(crimeData.pca1$x)
plot(crimeData.y.norm, pcs$PC1)

ols.data <- cbind(crimeData.y.norm, pcs)
lmodel <- lm(crimeData.y.norm ~ ., data = ols.data)

summary(lmodel)

beta.Z <- as.matrix(lmodel$coefficients[2:123])
V <- as.matrix(crimeData.pca1$rotation)
beta.X <- V %*% beta.Z
beta.X

lmodel.none <- lm(y ~ ., data = crimeData)
summary(lmodel.none)

crimeData.test.norm <- data.Normalization(crimeData.test[,1:122])
crimeData.test.y.norm <- data.Normalization(crimeData.test[,123])

fit <- pcr(crimeData.y.norm ~., data = cbind(crimeData.norm, crimeData.y.norm))

y.pred.test1 <- predict(fit,newdata=crimeData.test.norm)
y.pred.test1%>%  dim

y.pred.test1 <- y.pred.test1[1:544,1,122]
dim(y.pred.test1) <- c(544,1)

pred.test2 <- as.matrix(crimeData.test.norm)
y.pred.test2 <- pred.test2 %*% beta.X

y.pred.test1 %>% head
y.pred.test2 %>% head

validationplot(fit, val.type="R2", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

fit2 <- pcr(crimeData.y.norm ~., data = cbind(crimeData.norm, crimeData.y.norm), validation="CV")

validationplot(fit2, val.type="RMSEP", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

fit2 <- pcr(crimeData.y.norm ~., ncomp=8, data = cbind(crimeData.norm, crimeData.y.norm), validation = "CV")

# Plot the R2
validationplot(fit2, val.type = "R2")

predplot(fit2)

coefplot(fit2)

coef(fit2)

y.pred.test12 <- predict(fit2,newdata=crimeData.test.norm)
y.pred.test12%>%  dim

y.pred.test12 <- y.pred.test12[1:544,1,8]
dim(y.pred.test12) <- c(544,1)

