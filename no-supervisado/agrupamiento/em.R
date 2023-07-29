library("mclust")

data(diabetes)
summary(diabetes)

class.d = diabetes$class
table(class.d)

X = diabetes[,-1]
clPairs(X, class.d)

fit <- Mclust(X)
fit

plot(fit, what = "classification")
table(class.d, factor(fit$classification,labels=c("Normal","Chemical","Overt")))
