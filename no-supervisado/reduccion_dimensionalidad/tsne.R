library(dslabs)
library(Rtsne)

mnist <- read_mnist()

training_set <- as.data.frame(mnist$train$images)
training_set$label <- as.factor(mnist$train$label)

numTrain <- 5000
set.seed(1)
rows <- sample(1:nrow(training_set), numTrain)
train <- training_set[rows,]

# using tsne
set.seed(1) # for reproducibility
tsne <- Rtsne(train[,-785], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)

colors = rainbow(length(unique(train$label)))
names(colors) = unique(train$label)
par(mgp=c(2.5,1,0))
plot(tsne$Y, t='n', main="tSNE", xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
text(tsne$Y, labels=train$label, col=colors[train$label])

pca = princomp(train[,-785])$scores[,1:2]
plot(pca, t='n', main="pca", "cex.main"=2, "cex.lab"=1.5)
text(pca, labels=train$label,col=colors[train$label])

mds <- train[,-785]%>%
  dist() %>%          
  cmdscale() %>%
  as_tibble(.name_repair = 'unique')

plot(mds, t='n', main="mds", "cex.main"=2, "cex.lab"=1.5)
text(mds, labels=train$label,col=colors[train$label])

tsne_plot <- function(perpl=30,iterations=500,learning=200){
  set.seed(1) # for reproducibility
  tsne <- Rtsne(train[,-785], dims = 2, perplexity=perpl, verbose=TRUE, max_iter=iterations, eta=learning)
  plot(tsne$Y, t='n', main = print(paste0("perplexity = ",perpl, ", max_iter = ",iterations, ", learning rate = ",learning)), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=1, "cex.lab"=1.5)
  text(tsne$Y, labels=train$label, col=colors[train$label])
}

perplexity_values <- c(2,5,30,50,100)
sapply(perplexity_values,function(i){tsne_plot(perpl=i)})

iteration_values <- c(10,50,100,1000)
sapply(iteration_values,function(i){tsne_plot(iterations=i)})

learning_values <- c(20,200,2000)
sapply(learning_values,function(i){tsne_plot(learning=i)})



