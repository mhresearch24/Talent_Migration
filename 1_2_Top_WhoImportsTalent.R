# Script: Graph Visualization – Importing Countries Network (1_1)
# Purpose: Visualize international research flows, highlighting the importers —
# i.e., focusing on the countries of research affiliations in (origin → affiliation) flows.

# -- Import prepared data: df_research_contributions, graph_initial_edges
source("0_DataToUse.R")

# Create a list of unique countries as graph nodes
nodes_1_2 <- data.frame(
  id = unique(c(df_research_contributions$Origin_Country, df_research_contributions$Research_Country)),
  label = unique(c(df_research_contributions$Origin_Country, df_research_contributions$Research_Country)),
  font.size = 50  # Size of node labels
)

# Prepare edges for visNetwork visualization
# Summarize data to count the number of flows between each origin and research country
edges_1_2 <- df_research_contributions %>%
  group_by(Origin_Country, Research_Country) %>%
  summarise(flow_count = n(), .groups = "drop") %>%
  rename(from = Origin_Country, to = Research_Country) %>%
  mutate(
    # Normalize flow count for edge width scaling
    flow_count_scaled = (flow_count - min(flow_count)) / (max(flow_count) - min(flow_count)),
    width = 1 + flow_count_scaled * 90  # Edge width will range from 1 to 91
  )

# -------------------- Highlight node size and color based on in-degree centrality
# In a directed graph, **input degree** (in-degree) is the number of edges directed *into* a node —
# in this context, it indicates how many research contributions a country has *received*.

g <- graph_from_data_frame(graph_initial_edges, directed = TRUE, vertices = nodes_1_2)

# Compute in-degree centrality (excluding and including self-loops)
# - Self-loops are cases where origin country == research country
nodes_1_2$input_degree_self_excluded <- degree(g, mode = "in", loops = FALSE)  # Excludes domestic contributions
nodes_1_2$input_degree_self_included <- degree(g, mode = "in", loops = TRUE)   # Includes domestic contributions

# Normalize input degree (excluding self-loops) to control node size in visualization
# Formula: scaled_value = min_size + (value - min) / (max - min) * range
nodes_1_2$size <- nodes_1_2$input_degree_self_excluded
nodes_1_2$size <- 30 + (nodes_1_2$input_degree_self_excluded - min(nodes_1_2$input_degree_self_excluded)) / 
  (max(nodes_1_2$input_degree_self_excluded) - min(nodes_1_2$input_degree_self_excluded)) * 120
# Nodes will scale between size 30 (lowest in-degree) to 150 (highest in-degree)

# Calculate percentage contribution of each country to total imports
nodes_1_2 <- nodes_1_2 %>%
  mutate(
    sum_values_self_exclu = sum(input_degree_self_excluded),
    sum_values_self_inclu = sum(input_degree_self_included)
  ) %>%
  mutate(
    percentage_self_exclu = round((input_degree_self_excluded / sum_values_self_exclu) * 100, 2),
    percentage_self_inclu = round((input_degree_self_included / sum_values_self_inclu) * 100, 2)
  )

# Highlight top 10 importing countries based on in-degree (excluding self-loops)
top_nodes_1_2 <- nodes_1_2 %>%
  arrange(desc(input_degree_self_excluded)) %>%
  head(10)

# Assign distinct color to top importing countries
nodes_1_2$color <- ifelse(nodes_1_2$id %in% top_nodes_1_2$id, "#259073", "lightblue")

# Customize font style for top nodes
nodes_1_2 <- nodes_1_2 %>%
  mutate(
    font.size = ifelse(id %in% top_nodes_1_2$id, 100, font.size),
    font.color = ifelse(id %in% top_nodes_1_2$id, "#97124b", "black")
  )

# Create tooltip content for each node
nodes_1_2$title <- paste(
  "<b>Research Hub:</b>", nodes_1_2$label,
  "<br><b>Total Imported Contributions (Excluding Domestic):</b>", nodes_1_2$input_degree_self_excluded, 
  "(", nodes_1_2$percentage_self_exclu, "% of total imported contributions)",
  "<br><b>Total Imported Contributions (Including Domestic):</b>", nodes_1_2$input_degree_self_included, 
  "(", nodes_1_2$percentage_self_inclu, "% of total imported contributions)"
)

# -------------------- Visualize the graph with visNetwork
g_1_2 <- visNetwork(nodes_1_2, edges_1_2) %>%
  visOptions(
    highlightNearest = TRUE,
    selectedBy = list(
      variable = "label",
      main = "Select a Country to Highlight",
      highlight = TRUE,
      style = "width: 210px; font-size: 14px;"
    )
  ) %>%
  visEdges(
    arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),  # Small arrowheads
    color = list(color = 'lightblue', highlight = 'green')
  ) %>%
  visNodes(
    size = nodes_1_2$size
  ) %>%
  visPhysics(
    solver = "forceAtlas2Based",  # Dynamic graph layout
    stabilization = FALSE,
    repulsion = -4000
  ) %>%
  visInteraction(
    dragNodes = TRUE,
    dragView = TRUE,
    zoomView = TRUE,
    tooltipDelay = 50
  )

# Display the final graph
g_1_2
