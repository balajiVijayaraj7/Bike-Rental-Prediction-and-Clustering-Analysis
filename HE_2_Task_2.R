set.seed(123)
#preprocessing ####
train_set <- read.csv("/Users/balajivijayaraj/Desktop/Statistical_Learning/HomeExercise_2/New Data/train_160523.csv")
test_set <- read.csv("/Users/balajivijayaraj/Desktop/Statistical_Learning/HomeExercise_2/New Data/test_160523.csv")

full_set <- rbind(train_set[,1:10],test_set)
full_set <- full_set[-1]

full_set$year <- year(full_set$datetime)
full_set$month <- month(full_set$datetime)
full_set$day <- day(full_set$datetime)
full_set$hour <- hour(full_set$datetime)
full_set$minute <- minute(full_set$datetime)

full_set <- full_set[-1]
full_set <- full_set[-13]

summary(full_set)
str(full_set)
# cat and num dataset ####
columns_to_remove <- c("temp", "atemp", "humidity", "windspeed")
cat_data <- full_set[, !(colnames(full_set) %in% columns_to_remove)]
num_data <- full_set[, !(colnames(full_set) %in% colnames(cat_data))]

# one-hot encoding ####
cat_data$season <- as.factor(cat_data$season )
cat_data$holiday <- as.factor(cat_data$holiday )
cat_data$workingday <- as.factor(cat_data$workingday )
cat_data$weather <- as.factor(cat_data$weather )
cat_data$year <- as.factor(cat_data$year )
cat_data$month <- as.factor(cat_data$month )
cat_data$day <- as.factor(cat_data$day )
cat_data$hour <- as.factor(cat_data$hour )

# Specify the columns to be one-hot encoded
columns_to_encode <- c("season", "year", "month", "weather")

# Perform one-hot encoding
encoded_data <- dummyVars(~., data = cat_data[, columns_to_encode], fullRank = TRUE)
encoded_data <- as.data.frame(predict(encoded_data, newdata = cat_data[, columns_to_encode]))

# Add columns for the reference levels
reference_levels <- sapply(cat_data[, columns_to_encode], levels)
for (col in columns_to_encode) {
  reference_level <- reference_levels[[col]][1]
  col_name <- paste0(col, ".", reference_level)
  encoded_data[[col_name]] <- ifelse(cat_data[[col]] == reference_level, 1, 0)
}

columns_to_encode_1 <- c("day", "hour")

# Perform one-hot encoding
encoded_data_1 <- dummyVars(~., data = cat_data[, columns_to_encode_1], fullRank = TRUE)
encoded_data_1 <- as.data.frame(predict(encoded_data_1, newdata = cat_data[, columns_to_encode_1]))

# Add columns for the reference levels
reference_levels <- sapply(cat_data[, columns_to_encode_1], levels)
for (col in columns_to_encode_1) {
  reference_level <- reference_levels[[col]][1]
  col_name <- paste0(col, ".", reference_level)
  encoded_data_1[[col_name]] <- ifelse(cat_data[[col]] == reference_level, 1, 0)
}

combined_encoded_data <- cbind(encoded_data, encoded_data_1, holiday = cat_data$holiday, workingday = cat_data$workingday)

combined_full_data <- cbind(combined_encoded_data, num_data)
colnames(combined_full_data)
combined_full_data$holiday <- as.numeric(combined_full_data$holiday)
combined_full_data$workingday <- as.numeric(combined_full_data$workingday)
str(combined_full_data)


# Perform PCA ####
# Scale the variables
scaled_data <- scale(combined_full_data)

pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Get the principal components
principal_components <- pca_result$x

# Get the standard deviations of the principal components
standard_deviations <- pca_result$sdev

# Get the proportion of variance explained by each principal component
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Calculate the cumulative variance explained
cumulative_variance <- cumsum(variance_explained)

# Print the cumulative variance explained
print(cumulative_variance)

str(cat_data)
summary(cat_data)

# Perform clustering using the first 51 principal components ####
num_components <- 52 # 90% variance explained
pca_components <- principal_components[, 1:num_components]
k_values <- 2:15 # Range of cluster numbers to try

# Initialize vector to store the within-cluster sum of squares
wss <- vector("numeric", length(k_values))

# Perform clustering for each value of k and calculate WCSS
for (k in k_values) {
  kmeans_result <- kmeans(pca_components, centers = k, nstart = 25, iter.max = 50, algorithm = "Hartigan-Wong")
  wss[k-1] <- kmeans_result$tot.withinss
}

# Plot the elbow plot
plot(k_values, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares (WCSS)",
     main = "Elbow Plot")

#clustering ####
k <- 4

kmeans_result <- kmeans(pca_components, centers = k, nstart = 10)

# Plot the clusters
fviz_cluster(kmeans_result, data = pca_components, geom = "point",
             frame.type = "norm", frame.alpha = 0.5,
             ggtheme = theme_minimal())

# Get the cluster assignments for each data point
cluster_assignments <- kmeans_result$cluster

# Print the cluster assignments
print(cluster_assignments)

# Plot the clusters
plot(full_set, col = cluster_assignments, pch = 16, main = "K-means Clustering with 4 Clusters")

# Add cluster information to the dataset
full_set_with_clusters_encoded <- cbind(full_set, Cluster = cluster_assignments)

train_set_with_cluster <- cbind(full_set_with_clusters_encoded[1:8708,], train_set[,11:13])
train_set_with_cluster <- train_set_with_cluster[,-14] # run twice


# Filter the dataset for each cluster ####
cluster1_data <- subset(train_set_with_cluster, Cluster == 1)
cluster2_data <- subset(train_set_with_cluster, Cluster == 2)
cluster3_data <- subset(train_set_with_cluster, Cluster == 3)
cluster4_data <- subset(train_set_with_cluster, Cluster == 4)

cluster1_data$season <- as.factor(cluster1_data$season )
cluster1_data$holiday <- as.factor(cluster1_data$holiday )
cluster1_data$workingday <- as.factor(cluster1_data$workingday )
cluster1_data$weather <- as.factor(cluster1_data$weather )
cluster1_data$year <- as.factor(cluster1_data$year )
cluster1_data$month <- as.factor(cluster1_data$month )
cluster1_data$day <- as.factor(cluster1_data$day )
cluster1_data$hour <- as.factor(cluster1_data$hour )

cluster2_data$season <- as.factor(cluster2_data$season )
cluster2_data$holiday <- as.factor(cluster2_data$holiday )
cluster2_data$workingday <- as.factor(cluster2_data$workingday )
cluster2_data$weather <- as.factor(cluster2_data$weather )
cluster2_data$year <- as.factor(cluster2_data$year )
cluster2_data$month <- as.factor(cluster2_data$month )
cluster2_data$day <- as.factor(cluster2_data$day )
cluster2_data$hour <- as.factor(cluster2_data$hour )

cluster3_data$season <- as.factor(cluster3_data$season )
cluster3_data$holiday <- as.factor(cluster3_data$holiday )
cluster3_data$workingday <- as.factor(cluster3_data$workingday )
cluster3_data$weather <- as.factor(cluster3_data$weather )
cluster3_data$year <- as.factor(cluster3_data$year )
cluster3_data$month <- as.factor(cluster3_data$month )
cluster3_data$day <- as.factor(cluster3_data$day )
cluster3_data$hour <- as.factor(cluster3_data$hour )

cluster4_data$season <- as.factor(cluster4_data$season )
cluster4_data$holiday <- as.factor(cluster4_data$holiday )
cluster4_data$workingday <- as.factor(cluster4_data$workingday )
cluster4_data$weather <- as.factor(cluster4_data$weather )
cluster4_data$year <- as.factor(cluster4_data$year )
cluster4_data$month <- as.factor(cluster4_data$month )
cluster4_data$day <- as.factor(cluster4_data$day )
cluster4_data$hour <- as.factor(cluster4_data$hour )

summary(cluster1_data)
summary(cluster2_data)
summary(cluster3_data)
summary(cluster4_data)

#checking the relation with count with boxplot ####
ggplot(train_set_with_cluster, aes(x = factor(Cluster), y = factor(hour))) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "atemp")


#season, atemp, month

# Hierarchical clustering with complete linkage ####
# Calculate the dissimilarity matrix
distance_matrix <- dist(combined_full_data)

# Perform hierarchical clustering using complete linkage
hclust_result <- hclust(distance_matrix, method = "complete")

# Plot the dendrogram
plot(hclust_result, main = "Hierarchical Clustering Dendrogram")
clusters <- cutree(hclust_result,4)
dummy <- copy(full_set)
dummy$cluster <- clusters


train_set_with_cluster <- cbind(dummy[1:8708,], train_set[,11:13])
train_set_with_cluster <- train_set_with_cluster[,-14] # run twice


# Filter the dataset for each cluster ####
cluster1_data <- subset(train_set_with_cluster, cluster == 1)
cluster2_data <- subset(train_set_with_cluster, cluster == 2)
cluster3_data <- subset(train_set_with_cluster, cluster == 3)
cluster4_data <- subset(train_set_with_cluster, cluster == 4)

cluster1_data$season <- as.factor(cluster1_data$season )
cluster1_data$holiday <- as.factor(cluster1_data$holiday )
cluster1_data$workingday <- as.factor(cluster1_data$workingday )
cluster1_data$weather <- as.factor(cluster1_data$weather )
cluster1_data$year <- as.factor(cluster1_data$year )
cluster1_data$month <- as.factor(cluster1_data$month )
cluster1_data$day <- as.factor(cluster1_data$day )
cluster1_data$hour <- as.factor(cluster1_data$hour )

cluster2_data$season <- as.factor(cluster2_data$season )
cluster2_data$holiday <- as.factor(cluster2_data$holiday )
cluster2_data$workingday <- as.factor(cluster2_data$workingday )
cluster2_data$weather <- as.factor(cluster2_data$weather )
cluster2_data$year <- as.factor(cluster2_data$year )
cluster2_data$month <- as.factor(cluster2_data$month )
cluster2_data$day <- as.factor(cluster2_data$day )
cluster2_data$hour <- as.factor(cluster2_data$hour )

cluster3_data$season <- as.factor(cluster3_data$season )
cluster3_data$holiday <- as.factor(cluster3_data$holiday )
cluster3_data$workingday <- as.factor(cluster3_data$workingday )
cluster3_data$weather <- as.factor(cluster3_data$weather )
cluster3_data$year <- as.factor(cluster3_data$year )
cluster3_data$month <- as.factor(cluster3_data$month )
cluster3_data$day <- as.factor(cluster3_data$day )
cluster3_data$hour <- as.factor(cluster3_data$hour )

cluster4_data$season <- as.factor(cluster4_data$season )
cluster4_data$holiday <- as.factor(cluster4_data$holiday )
cluster4_data$workingday <- as.factor(cluster4_data$workingday )
cluster4_data$weather <- as.factor(cluster4_data$weather )
cluster4_data$year <- as.factor(cluster4_data$year )
cluster4_data$month <- as.factor(cluster4_data$month )
cluster4_data$day <- as.factor(cluster4_data$day )
cluster4_data$hour <- as.factor(cluster4_data$hour )

summary(cluster1_data)
summary(cluster2_data)
summary(cluster3_data)
summary(cluster4_data)

#checking the relation with count with boxplot ####
ggplot(train_set_with_cluster, aes(x = factor(cluster), y = (humidity))) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "humidity")



