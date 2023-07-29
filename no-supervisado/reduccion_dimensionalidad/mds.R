data("swiss")
head(swiss)

# Load required packages
library(magrittr)
library(dplyr)
library(ggpubr)
# Cmpute MDS
mds <- swiss %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble(.name_repair = 'unique')
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(swiss),
          size = 1,
          repel = TRUE)

# K-means clustering
clust <- kmeans(mds, 3)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)
# Plot and color by groups
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(swiss),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

########################

res.cor <- cor(mtcars, method = "spearman")
mds.cor <- (1 - res.cor) %>%
  cmdscale() %>%
  as_tibble()
colnames(mds.cor) <- c("Dim.1", "Dim.2")
ggscatter(mds.cor, x = "Dim.1", y = "Dim.2", 
          size = 1,
          label = colnames(res.cor),
          repel = TRUE)
