library("e1071")

data(iris)
train <- iris
train$y <-ifelse(train[,5]=="setosa", 1, -1)

#train <- train[order(train$y, decreasing=TRUE),]

X <- as.matrix(train[,c("Petal.Length", "Petal.Width")])
y <- as.matrix(train$y)
#n <- dim(X)[1]

C <- 1e5     # Huge value forces hard margin problem
sv <- svm(y~Petal.Length+Petal.Width, data=train, kernel="linear", scale=FALSE, type="C-classification", cost=C)

W <- rowSums(sapply(1:length(sv$coefs), function(i) sv$coefs[i]*sv$SV[i,]))
svmline = c(sv$rho/W[2], -W[1]/W[2])

library(ggplot2)
plt <- ggplot(train, aes(x=Petal.Length, y=Petal.Width)) + 
  ggtitle("Solving the SVM") +
  geom_point(aes(fill=factor(y)), pch=21) +
  geom_abline(intercept=svmline[1], slope=svmline[2], size=1)+
  scale_fill_manual(values=c("red","blue"), guide='none')+
  theme(legend.position="bottom", legend.title=element_blank())
print(plt)

print(sprintf("svm number of nonzeros: %d", length(sv$coefs)))
print(sprintf("svm: Intercept: %f    Slope: %f", svmline[1], svmline[2]))

sv$SV

dim(sv$SV)[1]/(dim(train)[1]-1)
