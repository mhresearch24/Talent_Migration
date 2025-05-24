# ===============================================
# SCRIPT: Hierarchical Clustering of Research Countries
# ===============================================
# This script performs Agglomerative Hierarchical Clustering (AHC)
# on country-level co-authorship data based on processed publication information: 'affiliation or adress'

# OUTPUTS:
# - Dendrogram of hierarchical clustering
# - Optional: Enhanced dendrogram visualization

# ===============================================
# 1. Load Required Libraries
# ===============================================
packages_part2 <- c("vegan", "factoextra","dplyr")

# Install any missing packages
installed <- packages_part2 %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages_part2[!installed])
}

# Load the libraries
lapply(packages_part2, library, character.only = TRUE)

# ===============================================
# 2. Load Preprocessed Data
# ===============================================
# Assumes the data was pre-cleaned to include only publications
# with known country affiliations for all authors
load("Data/Data_for_hidden_collab.RData")

# ===============================================
# 3. Prepare Data for Clustering
# ===============================================
# Focus on paper ID, affiliated country, and author
data_for_clustering <- Data_for_hidden_collab %>%
  select(pmid, countryAffiliation, auteur)

# Use only complete cases (no missing affiliations)
data_for_clustering_complete_rows <- data_for_clustering[complete.cases(data_for_clustering), ]

# Build a dataframe of unique (paper_id, country) combinations
df <- data.frame(
  paper_id = data_for_clustering_complete_rows$pmid,
  research_country = data_for_clustering_complete_rows$countryAffiliation
) %>% 
  distinct()

# ===============================================
# 4. Create Binary Country-Paper Matrix
# ===============================================
# Rows: Countries
# Columns: Paper IDs
# Value: 1 if country is involved in the paper, 0 otherwise

# Get unique research countries and paper IDs
research_countries <- unique(df$research_country)
unique_papers <- unique(df$paper_id)

# Initialize binary matrix
binary_matrix <- data.frame(matrix(0, nrow = length(research_countries), ncol = length(unique_papers)))
colnames(binary_matrix) <- unique_papers
rownames(binary_matrix) <- research_countries

# Fill the matrix: mark 1 where a country appears in a paper
for (i in 1:nrow(df)) {
  paper <- df$paper_id[i]
  country <- df$research_country[i]
  binary_matrix[country, as.character(paper)] <- 1
}

# Optional: View the matrix (for inspection)
View(binary_matrix)

# ===============================================
# 5. Filter for Relevant Countries and Papers
# ===============================================
# - Remove countries involved in < 100 papers
# - Remove papers with only 1 contributing country

Filtred_matrix <- binary_matrix[
  rowSums(binary_matrix) >= 100,
  colSums(binary_matrix) > 1
]

final_matrix <- Filtred_matrix

# ===============================================
# 6. Create Distance Matrix and Apply AHC
# ===============================================
# Use Bray-Curtis dissimilarity (good for binary ecological data)
dist_matrix <- vegdist(final_matrix, method = "bray")

# Perform hierarchical clustering using average linkage
hc <- hclust(dist_matrix, method = "average") # Alternatives: "complete", "ward.D2", etc.

# ===============================================
# 7. Plot the Dendrogram
# ===============================================
plot(
  hc,
  main = "Hierarchical Clustering of Research Countries Based on Paper Collaboration",
  xlab = "Countries",
  ylab = "Distance",
  labels = rownames(final_matrix)
)

# ===============================================
# 8. Optional: Enhanced Dendrogram Visualization
# ===============================================
# Uncomment below to display advanced dendrogram with clusters

# fviz_dend(
#   hc, 
#   cex = 0.8, lwd = 0.8,
#   k = 20,  # Number of clusters to highlight
#   rect = TRUE,
#   k_colors = "jco",
#   rect_border = "jco",
#   rect_fill = TRUE,
#   type = "phylogenic",
#   repel = TRUE
# ) +
#   theme(plot.margin = margin(10, 10, 10, 10))
