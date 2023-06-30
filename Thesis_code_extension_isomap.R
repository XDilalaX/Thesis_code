####################################################################################################################################################################################
# Author: Dilara Genc (494808dg)
# Study: Econometrics and Operational Research

# This program shows the extension part of the thesis. This code consists of
# step 0 up until 4 for the research of the thesis for k-means and AGNES clustering
# with Isomap as its dimension reduction technique

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
install.packages("vegan")

library(readxl)
library(cluster)
library(fpc)
library(clValid)
library(clusterSim)
library(NbClust)
library(vegan)

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
# (7) (NON-Linear Data Transformation) Isomap
# Perform Isomap
isomap_jester_dataset_full_subset <- isomap(dist(jester_dataset_full_subset), k=3)

# Get the the low-dimensional representation and view the Isomap coordinates
isomap_jester_dataset_full_subset_coordinates <- isomap_jester_dataset_full_subset$points #reduced data set
print(isomap_jester_dataset_full_subset_coordinates)

####################################################################################################################################################################################
# Step 2: determine optimal cluster count (k) (formerly step 1)
####################################################################################################################################################################################
# NOTE: Set a seed for reproducibility, then the random processes
# will not be random anymore after running the code multiple times
set.seed(100)
optimal_cluster_count <- NbClust(data = isomap_jester_dataset_full_subset_coordinates, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

# NOTE: k= (optimal cluster count)
best_optimal_cluster_count <- optimal_cluster_count$Best.nc

####################################################################################################################################################################################
# step 3: Clustering with the transformed data sets 
####################################################################################################################################################################################
# (1) k-means with k = 3 (found in step 1, see other code)
k <- 2

# (vii) k-means with Isomap
kmeans_isomap <- kmeans(isomap_jester_dataset_full_subset_coordinates, centers = k, nstart = 1000)

# Get the cluster assignments for internal validation
cluster_assignments_kmeans_isomap <- kmeans_isomap$cluster


# (2) AGNES with k = 3(found in step 1)
# (vii) AGNES with Isomap
agnes_isomap <- agnes(isomap_jester_dataset_full_subset_coordinates, metric = "euclidian", method = "average")

# Get and view the cluster assignments 
cluster_agnes_isomap <- cutree(agnes_isomap, k=2)
print(cluster_agnes_isomap)

####################################################################################################################################################################################
# step 4: Internal cluster validation
####################################################################################################################################################################################
# (6) For k_means and Isomap combined
# (i) Dunn Index
dunn_index_kmeans_isomap <- dunn(clusters = cluster_assignments_kmeans_isomap, Data = dist(isomap_jester_dataset_full_subset_coordinates))

# (ii) Calinski-Harabasz Index
calinski_index_kmeans_isomap <- calinhara(dist(isomap_jester_dataset_full_subset_coordinates), cluster_assignments_kmeans_isomap, cn = max(cluster_assignments_kmeans_isomap))

# (iii) Silhouette Index
silhouette_kmeans_isomap <- silhouette(cluster_assignments_kmeans_isomap, dist(isomap_jester_dataset_full_subset_coordinates))
silhouette_index_kmeans_isomap <- mean(silhouette_kmeans_isomap[, 3])

# (iv) Davies-Bouldin index
cl1 <- pam(cluster_assignments_kmeans_isomap, 2) #cluster count
davies_index_kmeans_isomap <- index.DB(cluster_assignments_kmeans_isomap, cl1$clustering, d=dist(isomap_jester_dataset_full_subset_coordinates), centrotypes="centroids", p=2, q=1)$DB


# For AGNES and Isomap combined 
# (i) Dunn Index
dunn_index_agnes_isomap <- dunn(clusters = cluster_agnes_isomap, Data = dist(isomap_jester_dataset_full_subset_coordinates))

# (ii) Calinski-Harabasz Index
calinski_index_agnes_isomap <- calinhara(dist(isomap_jester_dataset_full_subset_coordinates), cluster_agnes_isomap, cn = max(cluster_agnes_isomap))

# (iii) Silhouette Index
silhouette_agnes_isomap <- silhouette(cluster_agnes_isomap, dist(isomap_jester_dataset_full_subset_coordinates))
silhouette_index_agnes_isomap <- mean(silhouette_agnes_isomap[, 3])

# (iv) Davies-Bouldin index
cl2 <- pam(cluster_agnes_isomap, 3) #cluster count
davies_index_agnes_isomap <- index.DB(cluster_agnes_isomap, cl2$clustering, d=dist(isomap_jester_dataset_full_subset_coordinates), centrotypes="centroids", p=2, q=1)$DB

# print kmeans internal validation metrics 
print(dunn_index_kmeans_isomap)
print(calinski_index_kmeans_isomap)
print(silhouette_index_kmeans_isomap)
print(davies_index_kmeans_isomap)

# print AGNES internal validation metrics 
print(dunn_index_agnes_isomap)
print(calinski_index_agnes_isomap)
print(silhouette_index_agnes_isomap)
print(davies_index_agnes_isomap)
