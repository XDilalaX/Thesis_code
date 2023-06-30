####################################################################################################################################################################################
# Author: Dilara Genc (494808dg)
# Study: Econometrics and Operational Research

# This program shows step 2 up until 4 for the research 
# of the thesis for k-means and AGNES clustering with Isomap as its dimension 
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

library(readxl)
library(vegan)
library(cluster)
library(fpc)
library(clValid)
library(clusterSim)

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
#jester_dataset_full_subset_change <- Jester_no_NA[random_rows, 1:100] 
#jester_dataset_full_subset <- Jester_no_NA[450:500, 1:100]

# Remove unneeded data
rm(jester_dataset_full)
rm(jester_dataset_full_change)
rm(jester_dataset_full_sample)

####################################################################################################################################################################################
#step 2: Data Transformation
####################################################################################################################################################################################
# (7) (NON-Linear Data Transformation) Isomap
# Perform Isomap
isomap_jester_dataset_full_subset <- isomap(dist(jester_dataset_full_subset), k=3)

# Get the the low-dimensional representation and view the Isomap coordinates
isomap_jester_dataset_full_subset_coordinates <- isomap_jester_dataset_full_subset$points #reduced data set
print(isomap_jester_dataset_full_subset_coordinates)

####################################################################################################################################################################################
# step 3: Clustering with the transformed data sets 
####################################################################################################################################################################################
# (1) k-means with k = 3 (found in step 1, see other code)
k <- 3

# (vii) k-means with Isomap
kmeans_isomap <- kmeans(isomap_jester_dataset_full_subset_coordinates, centers = k, nstart = 1000)

# Get the cluster assignments for internal validation
cluster_assignments_kmeans_isomap <- kmeans_isomap$cluster


# (2) AGNES with k = 3(found in step 1)
# (vii) AGNES with Isomap
agnes_isomap <- agnes(isomap_jester_dataset_full_subset_coordinates, metric = "euclidian", method = "average")

# Get and view the cluster assignments 
cluster_agnes_isomap <- cutree(agnes_isomap, k=3)
print(cluster_agnes_isomap)

# Get kmeans with t-SNE visualization
plot(isomap_jester_dataset_full_subset_coordinates, col = cluster_assignments_kmeans_isomap, main = "K-means - with Isomap", xlab = "C1", ylab = "C2", pch = 15)

# Get AGNES with t-SNE visualisation 
plot(isomap_jester_dataset_full_subset_coordinates, col = cluster_agnes_isomap, main = "AGNES - with Isomap", xlab = "C1", ylab = "C2", pch = 15)


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
cl1 <- pam(cluster_assignments_kmeans_isomap, 3)
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
cl2 <- pam(cluster_agnes_isomap, 3)
davies_index_agnes_isomap <- index.DB(cluster_agnes_isomap, cl2$clustering, d=dist(isomap_jester_dataset_full_subset_coordinates), centrotypes="centroids", p=2, q=1)$DB

# Print internal validation metrics for kmeans 
print(dunn_index_kmeans_isomap)
print(calinski_index_kmeans_isomap)
print(silhouette_index_kmeans_isomap)
print(davies_index_kmeans_isomap)

# Print internal validation metrics for AGNES
print(dunn_index_agnes_isomap)
print(calinski_index_agnes_isomap)
print(silhouette_index_agnes_isomap)
print(davies_index_agnes_isomap)