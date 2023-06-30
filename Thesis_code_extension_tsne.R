####################################################################################################################################################################################
# Author: Dilara Genc (494808dg)
# Study: Econometrics and Operational Research

# This program shows the extension part of the thesis. This code consists of
# step 0 up until 4 for the research of the thesis for k-means and AGNES clustering
# with t-SNE as its dimension reduction technique

####################################################################################################################################################################################
# Step 0: Import full data set
####################################################################################################################################################################################
#install and load packages 
install.packages("readxl")
install.packages("Rtsne")
install.packages("ggplot2")
install.packages("cluster")
install.packages("fpc")
install.packages("clValid")
install.packages("clusterSim")
install.packages("NbClust")

library(readxl)
library(Rtsne)
library(ggplot2)
library(cluster)
library(fpc)
library(clValid)
library(clusterSim)
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
# (5) (NON-Linear Data Transformation) t-SNE
# Perform t-SNE
tSNE_jester_dataset_full_subset <- Rtsne(jester_dataset_full_subset, dims = 2, perplexity = 10) # Set perplexity to a smaller value

# Get the the low-dimensional representation and view the t-SNE coordinates
tSNE_jester_dataset_full_subset_coordinates <- tSNE_jester_dataset_full_subset$Y #reduced data set
print(tSNE_jester_dataset_full_subset_coordinates)

############################################################
# EXTRA: t-SNE while playing with perplexities 
# Perform t-SNE, with perplexity = 4
tSNE_jester_dataset_full_subset_1 <- Rtsne(jester_dataset_full_subset, dims = 2, perplexity = 4) # Set perplexity to a smaller value

# Get the the low-dimensional representation and view the t-SNE coordinates
tSNE_jester_dataset_full_subset_coordinates_1 <- tSNE_jester_dataset_full_subset_1$Y #reduced data set
print(tSNE_jester_dataset_full_subset_coordinates_1)

# Perform t-SNE, with perplexity = 6
tSNE_jester_dataset_full_subset_2 <- Rtsne(jester_dataset_full_subset, dims = 2, perplexity = 6) # Set perplexity to a smaller value

# Get the the low-dimensional representation and view the t-SNE coordinates
tSNE_jester_dataset_full_subset_coordinates_2 <- tSNE_jester_dataset_full_subset_2$Y #reduced data set
print(tSNE_jester_dataset_full_subset_coordinates_2)

# Perform t-SNE, with perplexity = 8
tSNE_jester_dataset_full_subset_3 <- Rtsne(jester_dataset_full_subset, dims = 2, perplexity = 8) # Set perplexity to a smaller value

# Get the the low-dimensional representation and view the t-SNE coordinates
tSNE_jester_dataset_full_subset_coordinates_3 <- tSNE_jester_dataset_full_subset_3$Y #reduced data set
print(tSNE_jester_dataset_full_subset_coordinates_3)

####################################################################################################################################################################################
# Step 2: Optimal cluster count (formerly step 1)
####################################################################################################################################################################################
# NOTE: Set a seed for reproducibility, then the random processes
# will not be random anymore after running the code multiple times
set.seed(100)
optimal_cluster_count <- NbClust(data = tSNE_jester_dataset_full_subset_coordinates, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

# NOTE: k= (optimal cluster count)
best_optimal_cluster_count <- optimal_cluster_count$Best.nc

###################################################
# EXTRA: optimal cluster count with perplexity changes 
# Perplexity = 4 
set.seed(100)
optimal_cluster_count_1 <- NbClust(data = tSNE_jester_dataset_full_subset_coordinates_1, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

# NOTE: k= (optimal cluster count)
best_optimal_cluster_count_1 <- optimal_cluster_count_1$Best.nc

# Perplexity = 6 
set.seed(100)
optimal_cluster_count_2 <- NbClust(data = tSNE_jester_dataset_full_subset_coordinates_2, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

# NOTE: k= (optimal cluster count)
best_optimal_cluster_count_2 <- optimal_cluster_count_2$Best.nc

# Perplexity = 8 
set.seed(100)
optimal_cluster_count_3 <- NbClust(data = tSNE_jester_dataset_full_subset_coordinates_3, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

# NOTE: k= (optimal cluster count)
best_optimal_cluster_count_3 <- optimal_cluster_count_3$Best.nc

####################################################################################################################################################################################
# step 3: Clustering with the transformed data sets 
####################################################################################################################################################################################
# (1) k-means with k = 3 (found in step 1, see other code)
k <- 2

# (v) k-means with t-SNE
kmeans_tsne <- kmeans(tSNE_jester_dataset_full_subset_coordinates, centers = k, nstart = 1000)

# Get the cluster assignments for internal validation
cluster_assignments_kmeans_tsne <- kmeans_tsne$cluster


# (2) AGNES with k = 3(found in step 1)
# (v) AGNES with t-SNE
agnes_tsne <- agnes(tSNE_jester_dataset_full_subset_coordinates, metric = "euclidian", method = "average")

# Get and view the cluster assignments 
cluster_agnes_tsne <- cutree(agnes_tsne, k=2)
print(cluster_agnes_tsne)

#########################################################
# Kmeans and AGNES with different perplexities 
#########################################################
# Perplexity = 4
# (v) k-means with t-SNE
kmeans_tsne_1 <- kmeans(tSNE_jester_dataset_full_subset_coordinates_1, centers = k, nstart = 1000)

# Get the cluster assignments for internal validation
cluster_assignments_kmeans_tsne_1 <- kmeans_tsne_1$cluster

# Get kmeans with t-SNE visualization
plot(tSNE_jester_dataset_full_subset_coordinates_1, col =cluster_assignments_kmeans_tsne_1, main = "K-means - with t-SNE with perplexity = 4", xlab = "V1", ylab = "V2", pch = 15)

# (2) AGNES with k = 3(found in step 1)
# (v) AGNES with t-SNE
agnes_tsne_1 <- agnes(tSNE_jester_dataset_full_subset_coordinates_1, metric = "euclidian", method = "average")

# Get and view the cluster assignments 
cluster_agnes_tsne_1 <- cutree(agnes_tsne_1, k=3)
print(cluster_agnes_tsne_1)

# Get AGNES with t-SNE visualization
plot(tSNE_jester_dataset_full_subset_coordinates_1, col =cluster_agnes_tsne_1, main = "AGNES - with t-SNE with perplexity = 4", xlab = "V1", ylab = "V2", pch = 15)


# Perplexity = 6
# (v) k-means with t-SNE
kmeans_tsne_2 <- kmeans(tSNE_jester_dataset_full_subset_coordinates_2, centers = k, nstart = 1000)

# Get the cluster assignments for internal validation
cluster_assignments_kmeans_tsne_2 <- kmeans_tsne_2$cluster

# Get kmeans with t-SNE visualization
plot(tSNE_jester_dataset_full_subset_coordinates_2, col =cluster_assignments_kmeans_tsne_2, main = "K-means - with t-SNE with perplexity = 6", xlab = "V1", ylab = "V2", pch = 15)

# (2) AGNES with k = 3(found in step 1)
# (v) AGNES with t-SNE
agnes_tsne_2 <- agnes(tSNE_jester_dataset_full_subset_coordinates_2, metric = "euclidian", method = "average")

# Get and view the cluster assignments 
cluster_agnes_tsne_2 <- cutree(agnes_tsne_2, k=3)
print(cluster_agnes_tsne_2)

# Get AGNES with t-SNE visualization
plot(tSNE_jester_dataset_full_subset_coordinates_2, col =cluster_agnes_tsne_2, main = "K-means - with t-SNE with perplexity = 4", xlab = "V1", ylab = "V2", pch = 15)


# Perplexity = 8
# (v) k-means with t-SNE
kmeans_tsne_3 <- kmeans(tSNE_jester_dataset_full_subset_coordinates_3, centers = k, nstart = 1000)

# Get the cluster assignments for internal validation
cluster_assignments_kmeans_tsne_3 <- kmeans_tsne_3$cluster

# Get kmeans with t-SNE visualization
plot(tSNE_jester_dataset_full_subset_coordinates_3, col =cluster_assignments_kmeans_tsne_3, main = "K-means - with t-SNE with perplexity = 8", xlab = "V1", ylab = "V2", pch = 15)

# (2) AGNES with k = 3(found in step 1)
# (v) AGNES with t-SNE
agnes_tsne_3 <- agnes(tSNE_jester_dataset_full_subset_coordinates_3, metric = "euclidian", method = "average")

# Get and view the cluster assignments 
cluster_agnes_tsne_3 <- cutree(agnes_tsne_3, k=3)
print(cluster_agnes_tsne_3)

# Get AGNES with t-SNE visualization
plot(tSNE_jester_dataset_full_subset_coordinates_3, col =cluster_agnes_tsne_3, main = "AGNES - with t-SNE with perplexity = 8", xlab = "V1", ylab = "V2", pch = 15)


####################################################################################################################################################################################
# step 4: Internal cluster validation
####################################################################################################################################################################################
# (4) For k_means and t-SNE combined
# (i) Dunn Index
dunn_index_kmeans_tsne <-  dunn(clusters = cluster_assignments_kmeans_tsne, Data = dist(tSNE_jester_dataset_full_subset_coordinates))

# (ii) Calinski-Harabasz Index
calinski_index_kmeans_tsne <-  calinhara(dist(tSNE_jester_dataset_full_subset_coordinates), cluster_assignments_kmeans_tsne, cn = max(cluster_assignments_kmeans_tsne))

# (iii) Silhouette Index
silhouette_kmeans_tsne <- silhouette(cluster_assignments_kmeans_tsne, dist(tSNE_jester_dataset_full_subset_coordinates))
silhouette_index_kmeans_tsne <- mean(silhouette_kmeans_tsne[, 3])

# (iv) Davies-Bouldin index
cl1 <- pam(cluster_assignments_kmeans_tsne, 2) # cluster amount
davies_index_kmeans_tsne <- index.DB(cluster_assignments_kmeans_tsne, cl1$clustering, d=dist(tSNE_jester_dataset_full_subset_coordinates), centrotypes="centroids", p=2, q=1)$DB


# For AGNES and t-SNE combined
# (i) Dunn Index
dunn_index_agnes_tsne <- dunn(clusters = cluster_agnes_tsne, Data = dist(tSNE_jester_dataset_full_subset_coordinates))

# (ii) Calinski-Harabasz Index
ch_index_agnes_tsne <-   calinhara(dist(tSNE_jester_dataset_full_subset_coordinates), cluster_agnes_tsne, cn = max(cluster_agnes_tsne))

# (iii) Silhouette Index
silhouette_agnes_tsne <- silhouette(cluster_agnes_tsne, dist(tSNE_jester_dataset_full_subset_coordinates))
silhouette_index_agnes_tsne <- mean(silhouette_agnes_tsne[, 3])

# (iv) Davies-Bouldin index
cl2 <- pam(cluster_agnes_tsne, 2) #cluster amount
davies_agnes_tsne <- index.DB(cluster_agnes_tsne, cl2$clustering, d=dist(tSNE_jester_dataset_full_subset_coordinates), centrotypes="centroids", p=2, q=1)$DB

print(dunn_index_kmeans_tsne)
print(calinski_index_kmeans_tsne)
print(silhouette_index_kmeans_tsne)
print(davies_index_kmeans_tsne)

# print AGNES internal validation metrics 
print(dunn_index_agnes_tsne)
print(ch_index_agnes_tsne)
print(silhouette_index_agnes_tsne)
print(davies_agnes_tsne)

##################################################################
# Kmeans and AGNES internal validation with different perplexities 
##################################################################
# perplexity = 4
# (4) For k_means and t-SNE combined
# (i) Dunn Index
dunn_index_kmeans_tsne_1 <-  dunn(clusters = cluster_assignments_kmeans_tsne_1, Data = dist(tSNE_jester_dataset_full_subset_coordinates_1))

# (ii) Calinski-Harabasz Index
calinski_index_kmeans_tsne_1 <-  calinhara(dist(tSNE_jester_dataset_full_subset_coordinates_1), cluster_assignments_kmeans_tsne_1, cn = max(cluster_assignments_kmeans_tsne_1))

# (iii) Silhouette Index
silhouette_kmeans_tsne_1 <- silhouette(cluster_assignments_kmeans_tsne_1, dist(tSNE_jester_dataset_full_subset_coordinates_1))
silhouette_index_kmeans_tsne_1 <- mean(silhouette_kmeans_tsne_1[, 3])

# (iv) Davies-Bouldin index
cl1_1 <- pam(cluster_assignments_kmeans_tsne_1, 2)
davies_index_kmeans_tsne_1 <- index.DB(cluster_assignments_kmeans_tsne_1, cl1_1$clustering, d=dist(tSNE_jester_dataset_full_subset_coordinates_1), centrotypes="centroids", p=2, q=1)$DB


# For AGNES and t-SNE combined
# (i) Dunn Index
dunn_index_agnes_tsne_1 <- dunn(clusters = cluster_agnes_tsne_1, Data = dist(tSNE_jester_dataset_full_subset_coordinates_1))

# (ii) Calinski-Harabasz Index
ch_index_agnes_tsne_1 <-   calinhara(dist(tSNE_jester_dataset_full_subset_coordinates_1), cluster_agnes_tsne_1, cn = max(cluster_agnes_tsne_1))

# (iii) Silhouette Index
silhouette_agnes_tsne_1 <- silhouette(cluster_agnes_tsne_1, dist(tSNE_jester_dataset_full_subset_coordinates_1))
silhouette_index_agnes_tsne_1 <- mean(silhouette_agnes_tsne_1[, 3])

# (iv) Davies-Bouldin index
cl2_1 <- pam(cluster_agnes_tsne_1, 2)
davies_agnes_tsne_1 <- index.DB(cluster_agnes_tsne_1, cl2_1$clustering, d=dist(tSNE_jester_dataset_full_subset_coordinates_1), centrotypes="centroids", p=2, q=1)$DB

# Print internal validation metrics for kmeans
print(dunn_index_kmeans_tsne_1)
print(calinski_index_kmeans_tsne_1)
print(silhouette_index_kmeans_tsne_1)
print(davies_index_kmeans_tsne_1)

# Print internal validation metrics for AGNES
print(dunn_index_agnes_tsne_1)
print(ch_index_agnes_tsne_1)
print(silhouette_index_agnes_tsne_1)
print(davies_agnes_tsne_1)

########################################
# perplexity = 6
# (4) For k_means and t-SNE combined
# (i) Dunn Index
dunn_index_kmeans_tsne_2 <-  dunn(clusters = cluster_assignments_kmeans_tsne_2, Data = dist(tSNE_jester_dataset_full_subset_coordinates_2))

# (ii) Calinski-Harabasz Index
calinski_index_kmeans_tsne_2 <-  calinhara(dist(tSNE_jester_dataset_full_subset_coordinates_2), cluster_assignments_kmeans_tsne_2, cn = max(cluster_assignments_kmeans_tsne_2))

# (iii) Silhouette Index
silhouette_kmeans_tsne_2 <- silhouette(cluster_assignments_kmeans_tsne_2, dist(tSNE_jester_dataset_full_subset_coordinates_1))
silhouette_index_kmeans_tsne_2 <- mean(silhouette_kmeans_tsne_2[, 3])

# (iv) Davies-Bouldin index
cl1_2 <- pam(cluster_assignments_kmeans_tsne_2, 2)
davies_index_kmeans_tsne_2 <- index.DB(cluster_assignments_kmeans_tsne_2, cl1_2$clustering, d=dist(tSNE_jester_dataset_full_subset_coordinates_2), centrotypes="centroids", p=2, q=1)$DB


# For AGNES and t-SNE combined
# (i) Dunn Index
dunn_index_agnes_tsne_2 <- dunn(clusters = cluster_agnes_tsne_2, Data = dist(tSNE_jester_dataset_full_subset_coordinates_2))

# (ii) Calinski-Harabasz Index
ch_index_agnes_tsne_2 <-   calinhara(dist(tSNE_jester_dataset_full_subset_coordinates_2), cluster_agnes_tsne_2, cn = max(cluster_agnes_tsne_2))

# (iii) Silhouette Index
silhouette_agnes_tsne_2 <- silhouette(cluster_agnes_tsne_2, dist(tSNE_jester_dataset_full_subset_coordinates_2))
silhouette_index_agnes_tsne_2 <- mean(silhouette_agnes_tsne_2[, 3])

# (iv) Davies-Bouldin index
cl2_2 <- pam(cluster_agnes_tsne_2, 2)
davies_agnes_tsne_2 <- index.DB(cluster_agnes_tsne_2, cl2_2$clustering, d=dist(tSNE_jester_dataset_full_subset_coordinates_2), centrotypes="centroids", p=2, q=1)$DB

# Print internal validation metrics for kmeans
print(dunn_index_kmeans_tsne_2)
print(calinski_index_kmeans_tsne_2)
print(silhouette_index_kmeans_tsne_2)
print(davies_index_kmeans_tsne_2)

# Print internal validation metrics for AGNES
print(dunn_index_agnes_tsne_2)
print(ch_index_agnes_tsne_2)
print(silhouette_index_agnes_tsne_2)
print(davies_agnes_tsne_2)

########################################
# perplexity = 8
# (4) For k_means and t-SNE combined
# (i) Dunn Index
dunn_index_kmeans_tsne_3 <-  dunn(clusters = cluster_assignments_kmeans_tsne_3, Data = dist(tSNE_jester_dataset_full_subset_coordinates_3))

# (ii) Calinski-Harabasz Index
calinski_index_kmeans_tsne_3 <-  calinhara(dist(tSNE_jester_dataset_full_subset_coordinates_3), cluster_assignments_kmeans_tsne_3, cn = max(cluster_assignments_kmeans_tsne_3))

# (iii) Silhouette Index
silhouette_kmeans_tsne_3 <- silhouette(cluster_assignments_kmeans_tsne_3, dist(tSNE_jester_dataset_full_subset_coordinates_3))
silhouette_index_kmeans_tsne_3 <- mean(silhouette_kmeans_tsne_3[, 3])

# (iv) Davies-Bouldin index
cl1_3 <- pam(cluster_assignments_kmeans_tsne_3, 2)
davies_index_kmeans_tsne_3 <- index.DB(cluster_assignments_kmeans_tsne_3, cl1_3$clustering, d=dist(tSNE_jester_dataset_full_subset_coordinates_3), centrotypes="centroids", p=2, q=1)$DB

# For AGNES and t-SNE combined
# (i) Dunn Index
dunn_index_agnes_tsne_3 <- dunn(clusters = cluster_agnes_tsne_3, Data = dist(tSNE_jester_dataset_full_subset_coordinates_3))

# (ii) Calinski-Harabasz Index
ch_index_agnes_tsne_3 <-   calinhara(dist(tSNE_jester_dataset_full_subset_coordinates_3), cluster_agnes_tsne_3, cn = max(cluster_agnes_tsne_3))

# (iii) Silhouette Index
silhouette_agnes_tsne_3 <- silhouette(cluster_agnes_tsne_3, dist(tSNE_jester_dataset_full_subset_coordinates_3))
silhouette_index_agnes_tsne_3 <- mean(silhouette_agnes_tsne_3[, 3])

# (iv) Davies-Bouldin index
cl2_3 <- pam(cluster_agnes_tsne_3, 2)
davies_agnes_tsne_3 <- index.DB(cluster_agnes_tsne_3, cl2_3$clustering, d=dist(tSNE_jester_dataset_full_subset_coordinates_3), centrotypes="centroids", p=2, q=1)$DB

# Print internal validation metrics for kmeans
print(dunn_index_kmeans_tsne_3)
print(calinski_index_kmeans_tsne_3)
print(silhouette_index_kmeans_tsne_3)
print(davies_index_kmeans_tsne_3)

# Print internal validation metrics for AGNES
print(dunn_index_agnes_tsne_3)
print(ch_index_agnes_tsne_3)
print(silhouette_index_agnes_tsne_3)
print(davies_agnes_tsne_3)




