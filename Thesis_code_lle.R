####################################################################################################################################################################################
# Author: Dilara Genc (494808dg)
# Study: Econometrics and Operational Research

# This program shows step 2 up until 4 for the research 
# of the thesis for k-means clustering with LLE as its dimension 
# reduction technique

####################################################################################################################################################################################
# Step 0: Import full data set
####################################################################################################################################################################################
# Install and load older versions
# Download package from CRAN archive
url <- "https://cran.r-project.org/src/contrib/Archive/lle/lle_1.1.tar.gz"
pkgFile <- "lle_1.1.tar.gz"
download.file(url = url, destfile = pkgFile)

# Expand the zip file using whatever system functions are preferred
# look at the DESCRIPTION file in the expanded package directory
# Install dependencies list in the DESCRIPTION file
install.packages(c("ada", "ipred", "evd"))

# Install package
install.packages(pkgs=pkgFile, type="source", repos=NULL)

# install and load packages 
install.packages("readxl")
install.packages("fpc")
install.packages("cluster")
install.packages("remotes")
#install.packages("devtools")
#install_version("lle", version = "1.1.0", repos = "https://cran.r-project.org/src/contrib/Archive/lle/lle_1.1.tar.gz")
#install_packages("lle", version = "1.1")

#install.version("lle", "1.1")
#install.packages("lle")

library(readxl)
library(fpc)
library(cluster)
library(remotes)
library(lle)

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
# (6) (NON-Linear Data Transformation) LLE
# Perform LLE, with k = 2 (#clusters) and d = 2 (#dimensions)
lle_jester_dataset_full_subset <- lle(jester_dataset_full_subset, m = 2, k = 3, nnk = TRUE)

# Get the the low-dimensional representation and view the LLE coordinates
lle_jester_dataset_full_subset_coordinates <- lle_jester_dataset_full_subset$Y #reduced data set
print(lle_jester_dataset_full_subset_coordinates)

####################################################################################################################################################################################
# step 3: Clustering with the transformed data sets 
####################################################################################################################################################################################
# (1) k-means with k = 3 (found in step 1, see other code)
k <- 3

# (vi) k-means with LLE
kmeans_lle <- kmeans(lle_jester_dataset_full_subset_coordinates, centers = k, nstart = 1000)

# Get the cluster assignments for internal validation
cluster_assignments_kmeans_lle <- kmeans_lle$cluster


# (2) AGNES with k = 3(found in step 1)
# (vi) AGNES with LLE
agnes_lle <- agnes(lle_jester_dataset_full_subset_coordinates, metric = "euclidian", method = "average")

# Get and view the cluster assignments 
cluster_agnes_lle <- cutree(agnes_lle, k=3)
print(cluster_agnes_lle)

# Get kmeans with t-SNE visualization
plot(lle_jester_dataset_full_subset_coordinates, col =cluster_assignments_kmeans_lle, main = "K-means - with lle", xlab = "C1", ylab = "C2", pch = 15)

# Get AGNES with t-SNE visualisation 
plot(lle_jester_dataset_full_subset_coordinates, col = cluster_agnes_lle, main = "AGNES - with lle", xlab = "C1", ylab = "C2", pch = 15)


####################################################################################################################################################################################
# step 4: Internal cluster validation
####################################################################################################################################################################################
# (1) For k_means and LLE combined:   
# (i) Dunn Index
dunn_index_kmeans_lle <- dunn(clusters = cluster_assignments_kmeans_lle, Data = dist(lle_jester_dataset_full_subset_coordinates))

# (ii) Calinski-Harabasz Index
calinski_index_kmeans_lle <- calinhara(dist(lle_jester_dataset_full_subset_coordinates), cluster_assignments_kmeans_lle, cn = max(cluster_assignments_kmeans_lle))

# (iii) Silhouette Index
silhouette_kmeans_lle <- silhouette(cluster_assignments_kmeans_lle, dist(lle_jester_dataset_full_subset_coordinates))
silhouette_index_kmeans_lle <- mean(silhouette_kmeans_lle[, 3]) 

# (iv) Davies-Bouldin Index
cl1 <- pam(cluster_assignments_kmeans_lle, 3) # Cluster amount
davies_index_kmeans_lle <- index.DB(cluster_assignments_kmeans_lle, cl1$clustering, d=dist(lle_jester_dataset_full_subset_coordinates), centrotypes="centroids", p=2, q=1)$DB

# (1) For AGNES and LLE combined:   
# (i) Dunn Index
dunn_index_agnes_lle <- dunn(clusters = cluster_agnes_lle, Data = dist(lle_jester_dataset_full_subset_coordinates))

# (ii) Calinski-Harabasz Index
calinski_index_agnes_lle <- calinhara(dist(lle_jester_dataset_full_subset_coordinates), cluster_agnes_lle, cn = max(cluster_agnes_lle))

# (iii) Silhouette Index
silhouette_agnes_lle <- silhouette(cluster_agnes_lle, dist(lle_jester_dataset_full_subset_coordinates))
silhouette_index_agnes_lle <- mean(silhouette_agnes_lle[, 3]) 

# (iv) Davies-Bouldin Index
cl2 <- pam(cluster_agnes_lle, 3) # Cluster amount
davies_index_agnes_lle <- index.DB(cluster_agnes_lle, cl2$clustering, d=dist(lle_jester_dataset_full_subset_coordinates), centrotypes="centroids", p=2, q=1)$DB

# Print internal validation metrics for kmeans 
print(dunn_index_kmeans_lle)
print(calinski_index_kmeans_lle)
print(silhouette_index_kmeans_lle)
print(davies_index_kmeans_lle)

# Print internal validation metrics for AGNES
print(dunn_index_agnes_lle)
print(calinski_index_agnes_lle)
print(silhouette_index_agnes_lle)
print(davies_index_agnes_lle)
