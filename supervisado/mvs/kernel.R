library(tidyverse) # data wrangling and plotting
library(mlbench) # spirals dataset
library(e1071) # for svm algorithm
library(fdm2id) #two moons and targets datasets
library(kableExtra) #html tables
library(gridExtra) #array of graphical objects (grobs)

#iris (linear frontier)
iris <- iris %>% rename("X" = "Sepal.Length", "Y" = "Sepal.Width") %>% mutate(Class = as.factor(ifelse(Species == "setosa", "Class 1", "Class 2")))

#two moons
two_moons <- data.twomoons(sigma = 0.1, graph = FALSE)

#target
target <- data.target2(initn = 600, graph = FALSE)

#spirals
spirals <- mlbench.spirals(n=600, cycles=2, sd=0.05)
spirals <- data.frame(X=spirals$x[,1], Y=spirals$x[,2], Class=spirals$classes)
levels(spirals$Class) <- c("Class 1", "Class 2")

accuracy <- sapply(list(iris, two_moons, target, spirals), function(df){
  
  models <- lapply(list("linear", "poly", "radial", "sigmoid"), function(meth){
    return(svm(Class ~ X + Y, df, kernel=meth, prob=TRUE))
  })
  
  names(models) <- c("linear", "poly", "radial", "sigmoid")
  
  accuracy <- sapply(models, function(x) sum(x$fitted == df$Class)/nrow(df))
  names(accuracy) <- names(models)
  return(accuracy)
  
})

accuracy <- data.frame(accuracy)
colnames(accuracy) <- c("iris", "moons", "target", "spirals")

accuracy %>%
  kable(digits = 3) %>% kable_styling(bootstrap_options = "striped", full_width = F)

kernel_plot <- function(df){
  grid <- expand.grid(X=seq(min(df$X), max(df$X), 0.05), Y=seq(min(df$Y), max(df$Y), 0.05))
  
  svm <- lapply(list("linear", "poly", "radial", "sigmoid"), function(x){
    return(svm(Class ~ X + Y, df, kernel=x, prob=TRUE))
  })
  names(svm) <- c("linear", "poly", "radial", "sigmoid")
  
  #grid values
  pred <- sapply(svm, function(x){
    pr <- attr(predict(x, grid, probability=TRUE), "probabilities")
    return(pr[ , which(attr(pr, "dimnames")[[2]] == "Class 1")])
  }) 
  pred_cut <- apply(pred, 2, function(x) cut(x, breaks = 4))
  grid <- cbind(grid, pred_cut)
  
  plot_base <- ggplot(df) + 
    geom_point(aes(X, Y, col=Class), show.legend = FALSE) +
    scale_color_manual(values=c("#0000FF", "#FF0000")) +
    theme_minimal()
  
  plots <- lapply(names(svm), function(x){
    plot_base +
      geom_tile(data=grid, aes(X, Y, fill=grid[, x]), show.legend = FALSE, alpha=0.5) + 
      scale_fill_manual(values=c("#FF0000", "#FF9999", "#99CCFF", "#0000FF")) + 
      annotate(geom="label", x=min(grid$X), y=max(grid$Y), label=x, hjust="inward", vjust="inward")
  })
  
  outcome <- list(plot_base)
  outcome <- c(outcome, plots)
  
  names(outcome) <- c("base", "linear", "poly", "radial", "sigmoid")
  return(outcome)
  
}

grid.arrange(grobs = c(kernel_plot(iris), kernel_plot(target), kernel_plot(two_moons), kernel_plot(spirals)), ncol=5)
