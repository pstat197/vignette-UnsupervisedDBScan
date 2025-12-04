# Loading libraries and personality trait data
library(tidyverse)
library(ggplot2)
library(knitr)
library(kableExtra)
library(dbscan)
library(cluster)
library(here)
library(factoextra)

# Loading the COP K-means function
source("Scripts/ckmeans.R")

personality <- read.csv("data/raw/personality_synthetic_dataset.csv")

#################################
# Supervised clustering methods #
#################################

## Partitioning around medoids ##

personality_PAM <- personality |>
  # Turning personality_type into a factor for categorization
  mutate(
    personality_type = as.factor(personality_type)
  )

# Fitting a PAM model
pam_clustered <- pam(personality_PAM, 3, diss = FALSE,
                     metric = "euclidean")

# Plotting the PAM results
plot(pam_clustered, main = "PAM Cluster Plot for Personality Data",
     which = 1)

# Inspecting the attributes of the PAM model
pam_clustered$medoids # Medoids

pam_clustered$clusinfo # Clustering info

names(pam_clustered$silinfo) # Attributes of the sihouette data

pam_clustered$silinfo$clus.avg.widths # Average width of silhouettes

# Cluster assignments vs. ground truth 
table(pam_clustered$clustering, personality$personality_type)

## COP K-Means ##

set.seed(11302025)

# keep only numeric features
personality_num <- as.matrix(personality[ , -1])

# Getting the must-link pairs for COP K-means
ext <- which(personality$personality_type == "Extrovert")
int <- which(personality$personality_type == "Introvert")
amb <- which(personality$personality_type == "Ambivert")

must_link_pairs <- rbind(
  sample(ext, 2),
  sample(int, 2),
  sample(amb, 2)
)
must_link_pairs

# Getting the can't-link pairs for COP K-means
cant_link_pairs <- rbind(
  c(sample(ext, 1), sample(int, 1)),
  c(sample(int, 1), sample(amb, 1)),
  c(sample(ext, 1), sample(amb, 1))
)
cant_link_pairs

# Running the COP K-means algorithm using the ckmeans2 function
ck_clusters <- ckmeans2(
  data = personality_num,
  k = 3,
  mustLink = must_link_pairs,
  cantLink = cant_link_pairs,
  maxIter = 50
)

# Assigned clusters vs ground truth comparison
table(ck_clusters, personality$personality_type)

# PCA for plotting clustering results
pca <- prcomp(personality_num, scale. = TRUE)
pca_df <- as.data.frame(pca$x[,1:2]) |>
  mutate(cluster = factor(ck_clusters))

ggplot(pca_df, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "COP K-Means Clustering (PCA Projection)",
    subtitle = "First two principal components",
    color = "Cluster"
  ) +
  theme_bw()

# Displaying various attributes of the COP K-means clusters

# Cluster sizes
table(ck_clusters)

# Finding cluster centres
cluster_centers <- aggregate(personality_num, 
                             by = list(cluster = ck_clusters), 
                             FUN = mean)

cluster_centers

# Comparing differences across personality types
dist_mat <- dist(personality_num)

sil <- silhouette(ck_clusters, dist_mat)

plot(sil, main = "Silhouette Plot for COP K-Means")

###################################
# Unsupervised clustering methods #
###################################

## DBSCan ##

# Removing personality types for unsupervised clustering
personality_cluster <- personality %>% select(-personality_type)

# Generating a knee plot to determine the optimal epsilon value for clustering
set.seed(1)
min_pts = 2*dim(personality_cluster)[2]
kNNdistplot(personality_cluster, k = min_pts)

# We get that 7 is the optimal epsilon value so we run DBSCan with eps = 7
set.seed(1)
pers_dbscan <- dbscan(personality_cluster, eps = 7, minPts = min_pts)
pers_dbscan

# Visualizing the clustered results using PCA and the factoextra package
pers_combined <- personality %>% 
  mutate(cluster = pers_dbscan$cluster) %>% 
  filter(cluster != 0)

fviz_cluster(list(clusters = pers_combined[,31], data = pers_combined[,2:30]), 
             geom = "point", ellipse = TRUE, 
             main = "DBScan Clustering w/o noise values") + 
  ggplot2::aes(shape = as.factor(pers_combined[,1]))

pers_combined %>% group_by(cluster) %>% summarise(max(personality_type))

## Unsupervised K-means ##

# Keep only numeric columns of data and remove missing numeric values
dat_num <- personality %>%
  select(where(is.numeric)) %>% 
  drop_na()

dat_scaled <- scale(dat_num)

# Choose the value of k using elbow method
set.seed(20251126)
max_k <- 10

wss_df <- tibble(
  k = 1:max_k,
  tot_withinss = map_dbl(k, ~kmeans(dat_scaled, centers = .x, nstart = 20)$tot.withinss)
)

# Elbow plot
ggplot(wss_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow plot for K-means",
       x = "Number of clusters (k)",
       y = "Total within-cluster sum of squares")

# Choose the value of k using average silhouette width method
set.seed(20251126)

sil_df <- tibble(
  k = 2:max_k,
  sil_width = map_dbl(k, ~{
    km <- kmeans(dat_scaled, centers = .x, nstart = 20)
    ss <- silhouette(km$cluster, dist(dat_scaled))
    mean(ss[, "sil_width"])
  })
)

# This shows the ideal number of groups is 2
ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  geom_point() +
  labs(title = "Average silhouette width by k",
       x = "Number of clusters (k)",
       y = "Average silhouette width")

# Fitting the unsupervised k-means model with k = 2
set.seed(20251127)

k_best <- 2
km_final <- kmeans(dat_scaled, centers = k_best, nstart = 50)

# Adding assigned clusters to data for plotting results
dat_clusters <- dat_num %>%
  filter(complete.cases(dat_num)) %>%
  mutate(cluster = factor(km_final$cluster))

# Cluster sizes
cluster_sizes <- dat_clusters %>%
  count(cluster, name = "n")

kable(cluster_sizes)

# Summarise cluster results, revealing two large clustering types
cluster_summary <- dat_clusters %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, .names = "mean_{.col}"),
            .groups = "drop")

print(cluster_summary)

# Use PCA to visualize results of clustering along two principal dimensions
pca <- prcomp(dat_scaled, center = TRUE, scale. = TRUE)

pca_df <- as_tibble(pca$x[, 1:2]) %>%
  rename(PC1 = 1, PC2 = 2) %>%
  mutate(cluster = dat_clusters$cluster)

ggplot(pca_df, aes(x = PC1, y = PC2, colour = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "K-means clusters visualised in first two PCs") +
  theme_minimal()