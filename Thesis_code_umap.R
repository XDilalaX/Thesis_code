####################################################################################################################################################################################
# Author: Dilara Genc (494808dg)
# Study: Econometrics and Operational Research

# This program shows step 2 up until 4 for the research 
# of the thesis for k-means and AGNES clustering with umap as its dimension 
# reduction technique

####################################################################################################################################################################################
# Step 0: Import full data set install packages 
####################################################################################################################################################################################
#install and load packages 
install.packages("readxl")
install.packages("vegan")
install.packages("cluster")
install.packages("fpc")
install.packages("clValid")
install.packages("clusterSim")
install.packages("umap")

library(readxl)
library(vegan)
library(cluster)
library(fpc)
library(clValid)
library(clusterSim)
library(umap)

# NOTE: The data set consists of 73.421 observations (users) of 101 variables
jester_dataset_full <- read_excel("~/Econometrie/Econometrie Jaar 3/Thesis/Data/jester_dataset_full.xlsx")
id <- which(jester_dataset_full$user_id == 100) # Select complete cases 

# Take a subset because you do not want to use the user_id column in you analysis
jester_dataset_full_change <- jester_dataset_full[, 2:101]

# Specify the value to remove
remove <- 99

# Replace the 99 values with NA
jester_dataset_full_change[jester_dataset_full_change == remove] <- NA

# Omit the NA cases and obtain a matrix without NA
Jester_no_NA <- na.omit(jester_dataset_full_change)

# Take subset of 5000 samples 
set.seed(100) # Set the seed for reproducibility

# Get random row indices
random_rows <- sample(nrow(Jester_no_NA), size = 50, replace = FALSE)

# Take the random subset of the matrix, 50x100=5000
jester_dataset_full_subset <- Jester_no_NA[random_rows, 1:100] 

# Remove unneeded data
rm(jester_dataset_full)
rm(jester_dataset_full_change)
rm(jester_dataset_full_sample)

####################################################################################################################################################################################
#step 2: Data Transformation
####################################################################################################################################################################################
# Perform dimension reduction using UMAP
reduced_data <- umap(jester_dataset_full_subset)

# Access the reduced coordinates
reduced_coordinates <- reduced_data$layout #reduced data set 

####################################################################################################################################################################################
# step 3: Clustering with the transformed data sets 
####################################################################################################################################################################################
# (1) k-means with k = 3 (found in step 1, see other code)
k <- 3

# (vii) k-means with umap
kmeans_umap <- kmeans(reduced_coordinates, centers = k, nstart = 1000)

# Get the cluster assignments for internal validation
cluster_assignments_kmeans_umap <- kmeans_umap$cluster

# (2) AGNES with k = 3(found in step 1)
# (vii) AGNES with umap
agnes_umap <- agnes(reduced_coordinates, metric = "euclidian", method = "average")

# Get and view the cluster assignments 
cluster_agnes_umap <- cutree(agnes_umap, k=3)
print(cluster_agnes_umap)

# Get kmeans with t-SNE visualization
plot(reduced_coordinates, col = cluster_assignments_kmeans_umap, main = "K-means - with Umap", xlab = "C1", ylab = "C2", pch = 15)

# Get AGNES with t-SNE visualisation 
plot(reduced_coordinates, col = cluster_agnes_umap, main = "AGNES - with Umap", xlab = "C1", ylab = "C2", pch = 15)


####################################################################################################################################################################################
# step 4: Internal cluster validation
####################################################################################################################################################################################
# (6) For k_means and umap combined
# (i) Dunn Index
dunn_index_kmeans_umap <- dunn(clusters = cluster_assignments_kmeans_umap, Data = dist(reduced_coordinates))

# (ii) Calinski-Harabasz Index
calinski_index_kmeans_umap <- calinhara(dist(reduced_coordinates), cluster_assignments_kmeans_umap, cn = max(cluster_assignments_kmeans_umap))

# (iii) Silhouette Index
silhouette_kmeans_umap <- silhouette(cluster_assignments_kmeans_umap, dist(reduced_coordinates))
silhouette_index_kmeans_umap <- mean(silhouette_kmeans_umap[, 3])

# (iv) Davies-Bouldin index
cl1 <- pam(cluster_assignments_kmeans_umap, 3)
davies_index_kmeans_umap <- index.DB(cluster_assignments_kmeans_umap, cl1$clustering, d=dist(reduced_coordinates), centrotypes="centroids", p=2, q=1)$DB

# For AGNES and umap combined 
# (i) Dunn Index
dunn_index_agnes_umap <- dunn(clusters = cluster_agnes_umap, Data = dist(reduced_coordinates))

# (ii) Calinski-Harabasz Index
calinski_index_agnes_umap <- calinhara(dist(reduced_coordinates), cluster_agnes_umap, cn = max(cluster_agnes_umap))

# (iii) Silhouette Index
silhouette_agnes_umap <- silhouette(cluster_agnes_umap, dist(reduced_coordinates))
silhouette_index_agnes_umap <- mean(silhouette_agnes_umap[, 3])

# (iv) Davies-Bouldin index
cl2 <- pam(cluster_agnes_umap, 3)
davies_index_agnes_umap <- index.DB(cluster_agnes_umap, cl2$clustering, d=dist(reduced_coordinates), centrotypes="centroids", p=2, q=1)$DB

# print kmeans internal validation metrics 
print(dunn_index_kmeans_umap)
print(calinski_index_kmeans_umap)
print(silhouette_index_kmeans_umap)
print(davies_index_kmeans_umap)

# print AGNES internal validation metrics 
print(dunn_index_agnes_umap)
print(calinski_index_agnes_umap)
print(silhouette_index_agnes_umap)
print(davies_index_agnes_umap)

