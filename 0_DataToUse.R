# ---------------------------------------------
# Script: Data Preparation for Research Flow Analysis
# Purpose: Prepare data for scripts prefixed with 1_1, 1_2, 2_1, 2_2
# Focus: Extracting clean origin-affiliation pairs for flows analyses
# ---------------------------------------------

# Load required libraries
packages <- c("dplyr", "visNetwork", "igraph", "DT",
              "networkD3", "htmltools", "countrycode")

# Install missing packages if necessary
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load all packages
lapply(packages, library, character.only = TRUE)

# ---------------------------------------------
# Load the processed data containing country info
# ---------------------------------------------
load("Data/Data_WithCountryNamesInfo.RData")

# Add continent information based on country names
Data_WithCountryNamesInfo$continents <- countrycode(
  sourcevar = Data_WithCountryNamesInfo$countryAffiliation, 
  origin = "country.name", 
  destination = "continent"
)

# Manually assign continent for Kosovo
Data_WithCountryNamesInfo <- Data_WithCountryNamesInfo %>%
  mutate(continents = ifelse(countryAffiliation == "Kosovo", "Europe", continents))

# ---------------------------------------------
# Prepare reduced data for graph analysis
# ---------------------------------------------
data_for_graph <- Data_WithCountryNamesInfo %>% 
  select(pmid, auteur, countryOrigin, countryAffiliation)

# Keep only complete rows where both origin and affiliation are available
data_for_graph_complete_rows <- data_for_graph[complete.cases(data_for_graph), ]

# Create a clean dataframe for author-country level contributions
df_research_contributions <- data.frame(
  PaperID = data_for_graph_complete_rows$pmid,        # Each paper may have multiple authors
  Origin_Country = data_for_graph_complete_rows$countryOrigin,
  Research_Country = data_for_graph_complete_rows$countryAffiliation
)

# Prepare initial edge list for network/graph analysis
graph_initial_edges <- df_research_contributions %>%
  select(Origin_Country, Research_Country)
