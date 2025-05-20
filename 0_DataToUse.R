# Load necessary library
packages <- c(
  "dplyr", "visNetwork", "igraph", "DT",
  "networkD3", "htmltools", "countrycode"
)

# Install any packages that are not already installed
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load all packages
lapply(packages, library, character.only = TRUE)


# load("DataExtraction/Data/Data_WithCountryNamesInfo.RData")
load("Data/Data_WithCountryNamesInfo.RData")
# ----------------add contient from country name
Data_WithCountryNamesInfo$continents <- countrycode(sourcevar = Data_WithCountryNamesInfo$countryAffiliation, 
                                                    origin = "country.name", 
                                                    destination = "continent")

Data_WithCountryNamesInfo <-Data_WithCountryNamesInfo %>%
  # ------lies Kosovo to  Europe
  mutate(continents=ifelse(countryAffiliation=="Kosovo","Europe",continents))
#----------------Prepare the data
data_for_graph <- Data_WithCountryNamesInfo%>% 
  select(c(pmid,auteur,countryOrigin,countryAffiliation))

data_for_graph_complete_rows <-data_for_graph[complete.cases(data_for_graph), ]
# data_for_graph_complete_rows <- data_for_graph_complete_rows[c(1:4200),]
df_research_contributions <- data.frame(
  PaperID =data_for_graph_complete_rows$pmid ,  # Each paper can have multiple authors
  Origin_Country = data_for_graph_complete_rows$countryOrigin,
  Research_Country = data_for_graph_complete_rows$countryAffiliation
)
length(unique(data_for_graph_complete_rows$auteur))


graph_initial_edges <- df_research_contributions %>%
  select(Origin_Country, Research_Country)


# ----------for collaboration Data , we select Data only when all origins and country affiliation are all available is 
Data_for_hidden_collab <- data_for_graph %>%
  group_by(pmid) %>%
  filter(all(!is.na(countryOrigin))&all(!is.na(countryAffiliation))) %>%
  ungroup()

Data_for_hidden_collab <-Data_for_hidden_collab[complete.cases(Data_for_hidden_collab), ]
