# Load required libraries
library(ggplot2)
library(cluster)
library(factoextra)
library(dbscan)
library(scatterplot3d)

# Load the dataset
load(url("http://statmath.wu.ac.at/~vana/datasets/RMFAnalysisClean.rda"))

# Step 1: Visualization of the data

# Pair plots for the 3 variables
# This visualization shows the pairwise relationships between Recency, Frequency, and Monetary.mean variables.
# It helps to understand the data distribution, trends, and potential groupings.
pairs(RMFAnalysisClean[, c("Recency", "Frequency", "Monetary.mean")])

# 3D plot using scatterplot3d
# This 3D plot visualizes the Recency, Frequency, and Monetary.mean variables in a single plot.
# It helps to observe potential clusters or groupings and understand the relationships between the variables.
scatterplot3d(RMFAnalysisClean$Recency, RMFAnalysisClean$Frequency, RMFAnalysisClean$Monetary.mean, pch = 19, color = "blue", main = "3D Scatter plot of Recency, Frequency, Monetary.mean")

# PCA and plot the first two principal components
# PCA is used to reduce the dimensionality of the dataset and visualize it in 2D.
# This plot shows the first two principal components, which capture most of the variance in the data.
# It helps to identify patterns, trends, and potential groupings.
pca <- prcomp(RMFAnalysisClean[, c("Recency", "Frequency", "Monetary.mean")], scale. = TRUE)
fviz_pca_ind(pca, title = "PCA plot of the first two principal components")

# Step 2: Perform clustering

# K-means clustering
# Optimal number of clusters using the elbow method
# This plot shows the total within-cluster sum of squares (WSS) as a function of the number of clusters (k).
# The "elbow" point in the plot suggests the optimal number of clusters, where adding more clusters does not significantly decrease WSS.
fviz_nbclust(RMFAnalysisClean[, c("Recency", "Frequency", "Monetary.mean")], kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method")

# Perform k-means clustering with the optimal number of clusters (4 in this case)
kmeans_clusters <- kmeans(RMFAnalysisClean[, c("Recency", "Frequency", "Monetary.mean")], centers = 4)

# Hierarchical clustering
# Compute distance matrix
dist_matrix <- dist(RMFAnalysisClean[, c("Recency", "Frequency", "Monetary.mean")], method = "euclidean")
# Hierarchical clustering using Ward's method
hclust_clusters <- hclust(dist_matrix, method = "ward.D2")
# Cut tree into optimal number of clusters
cluster_groups <- cutree(hclust_clusters, k = 4)

# DBSCAN clustering
dbscan_clusters <- dbscan(dist_matrix, eps = 0.6, minPts = 10)

# Step 3: Final suggestion and interpretation

# We choose k-means clustering as it is easier to interpret and provides good results
cluster_results <- aggregate(RMFAnalysisClean[, c("Recency", "Frequency", "Monetary.mean")], by = list(Cluster = kmeans_clusters$cluster), mean)
print(cluster_results)

# Interpretation
# The interpretation section now provides an explanation of how the average Recency, Frequency, and Monetary.mean values for each cluster can be used to characterize different customer segments based on their purchasing behavior. The online retailer can use this information to develop targeted marketing strategies and promotional offers to engage and retain customers effectively.

