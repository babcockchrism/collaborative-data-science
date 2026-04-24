
# Cluster Analysis of PCA Results

# This function performs k-means clustering on PCA results to group similar observations.
# It also determines the optimal number of clusters using the elbow method and generates plots.

# Initialize vector to store total within-cluster sum of squares (TWSS)
cluster_analysis <- function(pca, num_clusters = NA, max_clusters = 10, x = "PC1", y = "PC2", nstart = 10) { # segregates points from the principal component analysis into different clusters using kmeans clustering
  twss <- c()
  
  # Compute TWSS for different numbers of clusters (1 to max_clusters)
  for(i in 1:max_clusters) { # generates values for the total within-cluster sum of squares; important for this method of determining "optimal" number of clusters
    twss <- append(twss, kmeans(pca$pca, centers = i, nstart = nstart)$tot.withinss)
  }
  
  
  # Create data frame for plotting cluster evaluation
  clust_coords <- data.frame(1:max_clusters, twss)
  # Initialize optimal cluster count
  clust_opt <- 1
  
  # Determine optimal number of clusters using elbow method
  while(isTRUE(twss[clust_opt] > twss[clust_opt + 1])) { # for this method, the "optimal" number of clusters is determined by the "elbow" of the scree plot--in other words, what is the smallest number of clusters whose total within-cluster sum of squares is a local minimum?
    clust_opt <- clust_opt + 1
  }
  # If clusters are not specified, use optimal value
  if(is.na(num_clusters)) { # if no desired number of clusters is given, it defaults to the "optimal" number of clusters
    num_clusters <- clust_opt
  }
  
  # Perform final k-means clustering with selected number of clusters
  kmeans <- kmeans(pca$pca, centers = num_clusters, nstart = nstart)
  # Extract cluster assignments for each observation
  cluster_values <- kmeans$cluster
  # Calculate clustering quality (percentage of variance explained)
  qual <- 100 * kmeans$betweenss / kmeans$totss
  # Create elbow plot to visualize optimal number of clusters
  cluster_determine <- ggplot2::ggplot(data = clust_coords, aes(x = clust_coords[, 1], y = clust_coords[, 2])) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::xlab("Number of Clusters") +
    ggplot2::ylab("Total Within Sum of Squares") +
    ggplot2::geom_vline(xintercept = clust_opt, col = "red") +
    ggplot2::theme(panel.border = element_rect(colour = "black"))
  
  # Create scatterplot of PCA results colored by cluster assignment
  cluster_scatter <- ggplot2::ggplot(data = pca$pca, aes(x = pca$pca[, x], y = pca$pca[, y], color = cluster_values)) +
    ggplot2::geom_point() +
    ggplot2::xlab(x) +
    ggplot2::ylab(y) +
    ggplot2::theme(panel.border = element_rect(colour = "black"), aspect.ratio = 1)
  
  # Store outputs in a list
  output <- list(cluster_values, clust_opt, qual, cluster_determine, cluster_scatter)
  # Assign names to output elements
  names(output) <- c("cluster_values", "optimal_number_of_clusters", "quality", "optimal_cluster_plot", "pca_scatterplot")
  return(output)
}
