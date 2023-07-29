library(fpc)

# install.packages("devtools")
# devtools::install_github("kassambara/factoextra")

library(factoextra)
data("multishapes")

df <- multishapes[, 1:2]

set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)
plot(db, df, main = "DBSCAN", frame = FALSE)

