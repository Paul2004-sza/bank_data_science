library(tidyr)
library(dplyr)
library(readr)
library(cluster)
library(factoextra)
library(DBI)
library(dbscan)
library(ggplot2)
library(plotly)   
library(GGally)    
library(caret)     

customers <- read_csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/b_customers.csv", show_col_types = FALSE)
credit_scores <- read_csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/c_credit_scores.csv", show_col_types = FALSE)
loans <- read_csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/c_loans.csv", show_col_types = FALSE)
accounts <- read_csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/c_accounts.csv", show_col_types = FALSE)
transactions <- read_csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/processed/Transaction/transaction_statistics_per_account_with_customer_id.csv", show_col_types = FALSE)

customer_data <- customers %>%
  left_join(credit_scores, by = "customer_id") %>%
  left_join(
    loans %>%
      group_by(customer_id) %>%
      summarize(
        total_loans = n(), 
        defaults = sum(status == "default", na.rm = TRUE),
        paid_loans = sum(status == "paid", na.rm = TRUE),
        approved_loans = sum(status == "approved", na.rm = TRUE)
      ),
    by = "customer_id"
  ) %>%
  left_join(
    accounts %>%
      group_by(customer_id) %>%
      summarize(total_balance = sum(balance, na.rm = TRUE)),
    by = "customer_id"
  ) %>%
  left_join(transactions, by = "customer_id") %>%
  left_join(
    accounts %>%
      group_by(customer_id) %>%
      summarize(
        has_savings = as.integer(any(account_type == "Savings")),
        has_credit = as.integer(any(account_type == "Credit")),
        has_loan_account = as.integer(any(account_type == "Loan")),
        has_current = as.integer(any(account_type == "Current"))
      ),
    by = "customer_id"
  )

customer_data_clean <- customer_data %>%
  mutate(
    total_loans = replace_na(total_loans, 0),
    defaults = replace_na(defaults, 0),
    paid_loans = replace_na(paid_loans, 0),
    approved_loans = replace_na(approved_loans, 0),
    total_balance = replace_na(total_balance, 0),
    average_transaction = replace_na(average_transaction, 0),
    has_savings = replace_na(has_savings, 0),
    has_credit = replace_na(has_credit, 0),
    has_loan_account = replace_na(has_loan_account, 0),
    has_current = replace_na(has_current, 0)
  )

features_clean <- features %>%
  mutate_all(~ replace(., !is.finite(.), NA)) %>%  # Replace non-finite values (Inf, NaN) with NA
  drop_na()  # Remove rows with any NA values

#Check for zero variance columns and remove them
features_clean <- features_clean[, apply(features_clean, 2, var) != 0]

#Scale the data after cleaning
features_scaled <- scale(features_clean)

# Check for any missing, NaN, or Inf values after scaling
sum(is.na(features_scaled))       # Should be 0
sum(is.nan(features_scaled))      # Should be 0
sum(!is.finite(features_scaled))  # Should be 0


# K-Means Clustering
set.seed(123)
kmeans_result <- kmeans(features_scaled, centers = 4, nstart = 25)

customer_data_clean$kmeans_cluster <- NA
customer_data_clean$kmeans_cluster[as.numeric(rownames(features))] <- kmeans_result$cluster

# Visualize K-Means clusters
fviz_cluster(kmeans_result, data = features_scaled, geom = "point", ellipse.type = "norm", main = "K-Means Clustering")

# Hierarchical Clustering
dist_matrix <- dist(features_scaled)
hc_result   <- hclust(dist_matrix, method = "ward.D2")

# enlarge margins
par(mar = c(5, 4, 2, 1))
plot(hc_result,
     main = "Dendrogram for Hierarchical Clustering",
     xlab = "", sub = "", cex = 0.9)


hc_clusters <- cutree(hc_result, k = 4)
customer_data_clean$hc_cluster <- NA
customer_data_clean$hc_cluster[as.numeric(rownames(features))] <- hc_clusters

# DBSCAN Clustering 
dbscan_result <- dbscan(features_scaled, eps = 0.5, minPts = 5)
customer_data_clean$dbscan_cluster <- NA
customer_data_clean$dbscan_cluster[as.numeric(rownames(features))] <- dbscan_result$cluster

# PCA
pca_result <- prcomp(features_scaled, center = TRUE, scale. = TRUE)
pca_df <- data.frame(pca_result$x)
pca_df$kmeans_cluster <- factor(kmeans_result$cluster)

# 2D PCA Plot
ggplot(pca_df, aes(x = PC1, y = PC2, color = kmeans_cluster)) +
  geom_point() +
  labs(title = "PCA: K-Means Clustering", x = "PC1", y = "PC2") +
  theme_minimal()

# 3D PCA Plot
fig <- plot_ly(pca_df, x = ~PC1, y = ~PC2, z = ~PC3, color = ~kmeans_cluster,
               type = "scatter3d", mode = "markers") %>%
  layout(title = "3D Plot of PCA Components with K-Means Clusters")
fig

# Parallel Coordinates Plot
ggpairs(customer_data_clean %>%
          select(age, income, score, total_loans, total_balance, average_transaction, kmeans_cluster) %>%
          na.omit(), 
        aes(color = factor(kmeans_cluster)),
        title = "Parallel Coordinates for K-Means Clusters")

#Assign Segment Labels 
customer_data_clean$segment_label <- case_when(
  customer_data_clean$kmeans_cluster == 1 ~ "Affluent Savers",
  customer_data_clean$kmeans_cluster == 2 ~ "Young Borrowers",
  customer_data_clean$kmeans_cluster == 3 ~ "Frequent Spenders",
  customer_data_clean$kmeans_cluster == 4 ~ "Infrequent Users",
  TRUE ~ "Other"
)
# Export final labeled data
write_csv(customer_data_clean, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/processed/ML_data/customer_segments.csv")

# Check for issues in scaled features 
summary(features_scaled)./.
cat("Any NA? ", anyNA(features_scaled), "\n")
cat("Any NaN? ", any(is.nan(features_scaled)), "\n")
cat("Any Inf? ", any(is.infinite(features_scaled)), "\n")
