####################################################################################################################################################################################
# Author: Dilara Genc (494808dg)
# Study: Econometrics and Operational Research

# This program shows the code for the extension part of the thesis. This code consists of step 0 
# up until 4 for the research of the thesis for k-means and AGNES clustering with Umap as its dimension 
# reduction technique

####################################################################################################################################################################################
# Step 0: Import full data set
####################################################################################################################################################################################
#install and load packages 
install.packages("readxl")
install.packages("vegan")
install.packages("cluster")
install.packages("fpc")
install.packages("clValid")
install.packages("clusterSim")
install.packages("umap")
install.packages("NbClust")

library(readxl)
library(vegan)
library(cluster)
library(fpc)
library(clValid)
library(clusterSim)
library(umap)
library(NbClust)

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
# Step 1: Data transformation (formerly step 2)
####################################################################################################################################################################################
# Perform dimension reduction using UMAP
reduced_data <- umap(jester_dataset_full_subset)

# Access the reduced coordinates
reduced_coordinates <- reduced_data$layout #reduced data set 

####################################################################################################################################################################################
# Step 2: Optimal cluster count on transformed data (formerly step 1)
####################################################################################################################################################################################
# Determination optimal cluster count
# NOTE: Set a seed for reproducibility, then the random processes 
# will not be random anymore after running the code multiple times
set.seed(100) 

# Perform the NbClust analysis
nb_k <- NbClust(data = reduced_coordinates, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

# Obrain optimal cluster
optimal_cluster <- nb_k$Best.nc

# Obtain optimal cluster count -> results k =  
#nb_k <- NbClust(data = lle_jester_dataset_full_subset_coordinates, distance = "euclidean", min.nc = 0, max.nc = 2, method = "kmeans") # for kmeans clustering

####################################################################################################################################################################################
# step 3: Clustering with the transformed data sets 
####################################################################################################################################################################################
# (1) k-means with k = 3 (found in step 1, see other code)
k <- 2

# (vii) k-means with umap
kmeans_umap <- kmeans(reduced_coordinates, centers = k, nstart = 1000)

# Get the cluster assignments for internal validation
cluster_assignments_kmeans_umap <- kmeans_umap$cluster


# (2) AGNES with k = 3(found in step 1)
# (vii) AGNES with umap
agnes_umap <- agnes(reduced_coordinates, metric = "euclidian", method = "average")

# Get and view the cluster assignments 
cluster_agnes_umap <- cutree(agnes_umap, k=2)
print(cluster_agnes_umap)

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
cl1 <- pam(cluster_assignments_kmeans_umap, 2) #cluster amount
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
cl2 <- pam(cluster_agnes_umap, 2) #cluster amount
davies_index_agnes_umap <- index.DB(cluster_agnes_umap, cl2$clustering, d=dist(reduced_coordinates), centrotypes="centroids", p=2, q=1)$DB

# Print internal validation metrics for kmeans
print(dunn_index_kmeans_umap)
print(calinski_index_kmeans_umap)
print(silhouette_index_kmeans_umap)
print(davies_index_kmeans_umap)

# Print internal validation metrics for AGNES
print(dunn_index_agnes_umap)
print(calinski_index_agnes_umap)
print(silhouette_index_agnes_umap)
print(davies_index_agnes_umap)

