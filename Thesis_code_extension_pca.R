####################################################################################################################################################################################
# Author: Dilara Genc (494808dg)
# Study: Econometrics and Operational Research

# This program shows the extension part of the thesis. This code consists of
# step 0 up until 4 for the research of the thesis for k-means and AGNES clustering
# with PCA as its dimension reduction technique

####################################################################################################################################################################################
# Step 0: Import full data set
####################################################################################################################################################################################
#install and load packages 
install.packages("readxl")
install.packages("cluster")
install.packages("fpc")
install.packages("clValid")
install.packages("clusterSim")
install.packages("NbClust")

library(readxl)
library(cluster)
library(fpc)
library(clValid)
library(clusterSim)
library(NbClust)

# NOTE: The data set consists of 73.421 observations (users) of 101 variables
jester_dataset_full <- read_excel("~/Econometrie/Econometrie Jaar 3/Thesis/Data/jester_dataset_full.xlsx")
id <- which(jester_dataset_full$user_id == 100) # Select complete cases in order to ignore the 99 (NaN cases) 

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
# Step 1: Data transformation (formerly step 2)
####################################################################################################################################################################################
# (2) (Linear Data Transformation) PCA
# PCA technique with scaling the components
pca_jester_dataset_full_subset <- prcomp(jester_dataset_full_subset, scale. = TRUE)  # Use scale. = TRUE to scale the variables

# Get the principal components of the data set and view them
pca_principal_components <- pca_jester_dataset_full_subset$x #reduced data set
print(pca_principal_components)

####################################################################################################################################################################################
# Step 2: Optimal cluster count (formerly step 1)
####################################################################################################################################################################################
# NOTE: Set a seed for reproducibility, then the random processes
# will not be random anymore after running the code multiple times
set.seed(100)
optimal_cluster_count <- NbClust(data = pca_principal_components, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

# NOTE: k= (optimal cluster count)
best_optimal_cluster_count <- optimal_cluster_count$Best.nc

####################################################################################################################################################################################
# step 3: Clustering with the transformed data sets 
####################################################################################################################################################################################
# (1) k-means with k = 3 (found in step 1, see other code)
k <- 8

# (ii) k-means with PCA
kmeans_pca <- kmeans(pca_principal_components, centers = k, nstart = 1000)

# Get the cluster assignments for internal validation
cluster_assignments_kmeans_pca <- kmeans_pca$cluster


# (2) AGNES with k = 3 (found in step 1)
# (ii) AGNES with PCA
agnes_pca <- agnes(pca_principal_components, metric = "euclidian", method = "average")

# Get and view the cluster assignments 
cluster_agnes_pca <- cutree(agnes_pca, k=8)
print(cluster_agnes_pca)

####################################################################################################################################################################################
# step 4: Internal cluster validation
####################################################################################################################################################################################
# (1) For k_means and PCA combined:   
# (i) Dunn Index
dunn_index_kmeans_pca <- dunn(clusters = cluster_assignments_kmeans_pca, Data = dist(pca_principal_components))

# (ii) Calinski-Harabasz Index
calinski_index_kmeans_pca <- calinhara(dist(pca_principal_components), cluster_assignments_kmeans_pca, cn = max(cluster_assignments_kmeans_pca))

# (iii) Silhouette Index
silhouette_kmeans_pca <- silhouette(cluster_assignments_kmeans_pca, dist(pca_principal_components))
silhouette_index_kmeans_pca <- mean(silhouette_kmeans_pca[, 3]) 

# (iv) Davies-Bouldin Index
cl1 <- pam(cluster_assignments_kmeans_pca, 8) # Cluster amount
davies_index_kmeans_pca <- index.DB(cluster_assignments_kmeans_pca, cl1$clustering, d=dist(pca_principal_components), centrotypes="centroids", p=2, q=1)$DB


# For AGNES and PCA combined
# (i) Dunn Index 
dunn_index_agnes_pca <- dunn(clusters = cluster_agnes_pca, Data = dist(pca_principal_components))

# (ii) Calinski-Harabasz Index
ch_index_agnes_pca <- calinhara(dist(pca_principal_components), cluster_agnes_pca, cn = max(cluster_agnes_pca))

# (iii) Silhouette Index
silhouette_agnes_pca <- silhouette(cluster_agnes_pca, dist(pca_principal_components))
silhouette_index_agnes_pca <- mean(silhouette_agnes_pca[, 3])

# (iv) Davies-Bouldin index
cl2 <- pam(cluster_agnes_pca, 8) # Cluster amount
davies_index_agnes_pca <- index.DB(cluster_agnes_pca, cl2$clustering, d=dist(pca_principal_components), centrotypes="centroids", p=2, q=1)$DB

# print kmeans internal validation metrics 
print(dunn_index_kmeans_pca)
print(calinski_index_kmeans_pca)
print(silhouette_index_kmeans_pca)
print(davies_index_kmeans_pca)

# print AGNES internal validation metrics 
print(dunn_index_agnes_pca)
print(ch_index_agnes_pca)
print(silhouette_index_agnes_pca)
print(davies_index_agnes_pca)


