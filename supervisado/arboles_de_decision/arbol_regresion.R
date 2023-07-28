library(tree)
library(ISLR)
library(ggplot2)
library(caret)

# Accedemos a biblioteca de bateadores
attach(Hitters)

# Eliminamos datos nulos
Hitters<- na.omit(Hitters)
hist(Hitters$Salary)

# transformamos con logaritmo pra hacerlo más normal
Hitters$Salary <- log(Hitters$Salary)
hist(Hitters$Salary)

Hitters_plot <- Hitters
Hitters_plot$Salary_c <- cut(Hitters$Salary,4)

cbPalette <- c("blue", "green", "yellow", "red")

ggplot(Hitters_plot)+
  geom_point(aes(y=Hits,x=Years,color=Salary_c))+
  geom_rug(aes(y=Hits,x=Years),position='jitter')+
  scale_colour_manual(values=cbPalette)+
  theme_minimal()

tree.fit <- tree(Salary~Hits+Years, data=Hitters)
summary(tree.fit)


plot(tree.fit)
text(tree.fit, pretty=1)


split <- createDataPartition(y=Hitters$Salary, p=0.5, list=FALSE)

train <- Hitters[split,]
test <- Hitters[-split,]

#Create tree model
trees <- tree(Salary~., train)
plot(trees)
text(trees, pretty=0)

cv.trees <- cv.tree(trees)
plot(cv.trees)

prune.trees <- prune.tree(trees, best=4)
plot(prune.trees)
text(prune.trees, pretty=0)

yhat <- predict(prune.trees, test)
plot(yhat, test$Salary)
abline(0,1)
