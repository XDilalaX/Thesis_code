####################################################################################################################################################################################
# Author: Dilara Genc (494808dg)
# Study: Econometrics and Operational Research

####################################################################################################################################################################################
# Step 0: Import full data set
####################################################################################################################################################################################
#install and load packages
#install.packages("readxl")
library(readxl)

# NOTE: The data set consists of 73.421 observations (users) of 101 variables
jester_dataset_full <- read_excel("~/Econometrie/Econometrie Jaar 3/Thesis/Data/jester_dataset_full.xlsx")
id <- which(jester_dataset_full$user_id == 100) # select complete cases

#Take a subset because you do not want to use the user_id column in you analysis
jester_dataset_full_subset <- jester_dataset_full[id, 2:101]
rm(jester_dataset_full)
####################################################################################################################################################################################
# Step 1: determine optimal cluster count (k)
####################################################################################################################################################################################
#install and load packages
#install.packages("NbClust")
library(NbClust)

# NOTE: Set a seed for reproducibility, then the random processes
# will not be random anymore after running the code multiple times
set.seed(100)
optimal_cluster_count <- NbClust(data = jester_dataset_full_subset, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

# NOTE: k= (optimal cluster count)
best_optimal_cluster_count <- optimal_cluster_count$Best.nc

