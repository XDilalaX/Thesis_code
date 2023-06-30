####################################################################################################################################################################################
# Author: Dilara Genc (494808dg)
# Study: Econometrics and Operational Research

# This program shows step 2 up until 4 for the research 
# of the thesis for k-means and AGNES clustering with ICA as its dimension 
# reduction technique

####################################################################################################################################################################################
# Step 0: Import full data set and install packages
####################################################################################################################################################################################
#install and load packages 
install.packages("readxl")
install.packages("fastICA")
install.packages("cluster")
install.packages("fpc")
install.packages("clValid")
install.packages("clusterSim")

library(readxl)
library(fastICA)
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

# Remove unneeded data
rm(jester_dataset_full)
rm(jester_dataset_full_change)
rm(jester_dataset_full_sample)

####################################################################################################################################################################################
#step 2: Data Transformation
####################################################################################################################################################################################
# Perform ICA
ica_jester_dataset_full_subset <- fastICA(jester_dataset_full_subset, n.comp = 3)
#NOTE: how many components do you want? currently you chose 5 (Moet je dit aanpassen naar 3, omdat je k=3 in stap 3?)

# Get the independent components of the data set and view them
ica_independent_components <- ica_jester_dataset_full_subset$S #reduced data set
print(ica_independent_components)

####################################################################################################################################################################################
# step 3: Clustering with the transformed data sets 
####################################################################################################################################################################################
# (1) k-means with k = 3 (found in step 1, see other code)
k <- 3

# (iii) k-means with ICA 
kmeans_ica <- kmeans(ica_independent_components, centers = k, nstart = 1000)

# Get the cluster assignments for internal validation
cluster_assignments_kmeans_ica <- kmeans_ica$cluster


# (2) AGNES with k = 3(found in step 1)
# (iii) AGNES with ICA 
agnes_ica <- agnes(ica_independent_components, metric = "euclidian", method = "average")
# Get and view the cluster assignments 
cluster_agnes_ica <- cutree(agnes_ica, k=3)
print(cluster_agnes_ica)

# Get kmeans with t-SNE visualization
plot(ica_independent_components, col = cluster_assignments_kmeans_ica, main = "K-means - with ICA", xlab = "IC1", ylab = "IC2", pch = 15)

# Get AGNES with t-SNE visualisation 
plot(ica_independent_components, col = cluster_agnes_ica, main = "AGNES - with ICA", xlab = "IC1", ylab = "IC2", pch = 15)


####################################################################################################################################################################################
# step 4: Internal cluster validation
####################################################################################################################################################################################
# (2) For k_means and ICA combined
# (i) Dunn Index
dunn_index_kmeans_ica <- dunn(clusters = cluster_assignments_kmeans_ica, Data = dist(ica_independent_components))

# (ii) Calinski-Harabasz Index
calinski_index_kmeans_ica <- calinhara(dist(ica_independent_components), cluster_assignments_kmeans_ica, cn = max(cluster_assignments_kmeans_ica))

# (iii) Silhouette Index
silhouette_kmeans_ica <- silhouette(cluster_assignments_kmeans_ica, dist(ica_independent_components))
silhouette_index_kmeans_ica <- mean(silhouette_kmeans_ica[, 3])

# (iv) Davies-Bouldin Index
cl1 <- pam(cluster_assignments_kmeans_ica, 3)
davies_index_kmeans_ica <- index.DB(cluster_assignments_kmeans_ica, cl1$clustering, d=dist(ica_independent_components), centrotypes="centroids", p=2, q=1)$DB

# For AGNES and ICA combined 
# (i) Dunn Index
dunn_index_agnes_ica <- dunn(clusters = cluster_agnes_ica, Data = dist(ica_independent_components))

# (ii) Calinski-Harabasz Index
calinski_index_agnes_ica <- calinhara(dist(ica_independent_components), cluster_agnes_ica, cn = max(cluster_agnes_ica))

# (iii) Silhouette Index
silhouette_agnes_ica <- silhouette(cluster_agnes_ica, dist(ica_independent_components))
silhouette_index_agnes_ica <- mean(silhouette_agnes_ica[, 3])

# (iv) Davies-Bouldin Index
cl2 <- pam(cluster_agnes_ica, 3)
davies_agnes_ica <- index.DB(cluster_agnes_ica, cl2$clustering, d=dist(ica_independent_components), centrotypes="centroids", p=2, q=1)$DB

# print kmeans internal validation metrics 
print(dunn_index_kmeans_ica)
print(calinski_index_kmeans_ica)
print(silhouette_index_kmeans_ica)
print(davies_index_kmeans_ica)

# print AGNES internal validation metrics 
print(dunn_index_agnes_ica)
print(calinski_index_agnes_ica)
print(silhouette_index_agnes_ica)
print(davies_agnes_ica)