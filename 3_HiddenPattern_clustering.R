
library(factoextra)
library(vegan)  # for vegdist(data, method = "jaccard")
# load("DataExtraction/Data/Data_WithCountryNamesInfo.RData")
load("Data/Data_for_hidden_collab.RData")
#----------------Prepare the data
data_for_graph <- Data_for_hidden_collab%>% 
  select(c(pmid,countryAffiliation,auteur))

data_for_graph_complete_rows <-data_for_graph[complete.cases(data_for_graph), ]

# length(unique(data_for_graph_complete_rows))

df <- data.frame(
  paper_id =data_for_graph_complete_rows$pmid ,  # Each paper can have multiple authors
  # origin_country = data_for_graph_complete_rows$countryOrigin,
  research_country =  data_for_graph_complete_rows$countryAffiliation
) %>% 
  distinct()

# Step 1: Create a unique list of Research countries
# origin_countries <- unique(df$origin_country)
research_countries <- unique(df$research_country)
# Step 2: Create a binary matrix where each paper has 1 for research countries involved and 0 for others
binary_matrix <- data.frame(matrix(0, nrow = length(research_countries), ncol = length(unique(df$paper_id))))
colnames(binary_matrix) <- unique(df$paper_id)
rownames(binary_matrix) <- research_countries

# Step 3: Fill the matrix with 1s for research countries involved in each paper
for(i in 1:nrow(df)) {
  paper <- df$paper_id[i]
  research <- df$research_country[i]
  binary_matrix[research, as.character(paper)] <- 1
}



View(binary_matrix)


# --------
# to Filter infrequent papers
# Filtred_matrix <- binary_matrix[rowSums(binary_matrix)>2,(colSums(binary_matrix) > 2)]

# filter infrequent research countries : remove countries that are present just in less then 100 papers 
Filtred_matrix <- binary_matrix[rowSums(binary_matrix)>=100,(colSums(binary_matrix) > 1)]
# Step 4: Now transpose the binary matrix to invert it (countries as rows, papers as columns)
# binary_matrix_t <- t(binary_matrix)
final_matrix<-Filtred_matrix

length(unique(colnames(final_matrix)))
length(unique(rownames(final_matrix)))
# jaccard_dist <- vegdist(data, method = "jaccard")
# 
# # OR Hamming distance
# hamming_dist <- dist(final_matrix, method = "binary")
# Step 5: Compute the distance matrix (using Jaccard distance for binary data)
# dist_matrix <- dist(, method = "binary")
# View(as.matrix(dist_matrix))
# dist_matrix <- vegdist(final_matrix, method = "jaccard")
# cosine_similarity <- function(x) {
#   x <- as.matrix(x)
#   sim <- crossprod(x) / (sqrt(rowSums(x^2) %*% t(rowSums(x^2))))
#   return(as.dist(1 - sim)) # Convert to distance
# }
# dist_matrix <- cosine_similarity(binary_matrix)

dist_matrix <- vegdist(final_matrix, method = "bray")
# Step 6: Perform hierarchical clustering on the countries (rows)
hc <- hclust(dist_matrix, method = "average") #or complete
# rect.hclust(hc, k = 6, border = "red") 
# Step 7: Plot the dendrogram
# Labels will be the countries
plot(hc, main = "Hierarchical Clustering of Research Countries Based on Paper Collaboration", 
     xlab = "Countries", ylab = "Distance", labels = rownames(final_matrix))


# plot type 2
# 
# fviz_dend(hc, cex = 0.8, lwd = 0.8,k=20,
#           rect = TRUE, 
#           k_colors = "jco", 
#           rect_border = "jco", 
#           rect_fill = TRUE,
#           type = "phylogenic",
#           repel = TRUE)+
#   theme(plot.margin = margin(10, 10, 10, 10)) 
# 
# 
# 
# 
# vector_to_check <- c("Ethiopia","Russia")
# check_research_collab <- data_for_graph %>%
#   filter(countryAffiliation%in%vector_to_check) %>%
#   # select(-countryAffiliation)%>%
#   group_by(pmid) %>%
#   mutate(is_ther_collab= ifelse(length(unique(countryAffiliation))==2,"yes","no"))
# # 
# # ##check for research countries
# # vector_to_check2 <- c("Greece","United Kingdom")
# # check_research_collab_c_res <- data_for_graph %>% 
# #   filter(countryAffiliation %in%vector_to_check2) %>% 
# #   select(-countryOrigin)%>% 
# #   group_by(pmid) %>% 
# #   mutate(is_ther_collab= ifelse(all(vector_to_check2 %in% unique(countryAffiliation)),"yes","no"))
# check_research_collab_c_yes <- check_research_collab %>%
#   filter(is_ther_collab=="yes")
# # 
# # length(unique(check_research_collab_c_res$pmid))
#  check_jacc_dist <- 1-(length(unique(check_research_collab_c_yes$pmid))/length(unique( check_research_collab$pmid)))
# 
