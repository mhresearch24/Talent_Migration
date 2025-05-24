# -------------------------------------------------------------
# Script: Graph Visualization – Exporting Countries Network (1_1)
# Purpose: Visualize international research flows, highlighting the exporters — 
# i.e., focusing on origin countries in (origin → affiliation) flows.
# -------------------------------------------------------------

# Import prepared data: df_research_contributions, graph_initial_edges
source("0_DataToUse.R")

# -------------------------------------------------------------
# Create nodes: Unique countries involved in research flows
# -------------------------------------------------------------
nodes_1_1 <- data.frame(
  id = unique(c(df_research_contributions$Origin_Country, df_research_contributions$Research_Country)),
  label = unique(c(df_research_contributions$Origin_Country, df_research_contributions$Research_Country)),
  font.size = 50
)

# -------------------------------------------------------------
# Create edges: Directed flows from origin to affiliation country
# -------------------------------------------------------------
edges_1_1 <- df_research_contributions %>%
  group_by(Origin_Country, Research_Country) %>%
  summarise(flow_count = n(), .groups = "drop") %>%
  mutate(width = flow_count) %>%
  rename(from = Origin_Country, to = Research_Country) %>%
  mutate(
    flow_count_scaled = (flow_count - min(flow_count)) / (max(flow_count) - min(flow_count)),
    width = 1 + flow_count_scaled * 90  # Edge width ranges from 1 to 91
  )

# -------------------------------------------------------------
# Compute node output degree centrality (excluding and including self-loops)
# In a directed graph, the output degree of a node represents the number of outgoing edges from that node — 
# in this context, it reflects how many times a country has contributed research to other countries.

# -------------------------------------------------------------
g <- graph_from_data_frame(graph_initial_edges, directed = TRUE, vertices = nodes_1_1)

nodes_1_1$output_degree_self_excluded <- degree(g, mode = "out", loops = FALSE)
nodes_1_1$output_degree_self_included <- degree(g, mode = "out", loops = TRUE)

# Scale node size: min-max scaling from 30 to 150
nodes_1_1$size <- 30 + (nodes_1_1$output_degree_self_excluded - min(nodes_1_1$output_degree_self_excluded)) /
  (max(nodes_1_1$output_degree_self_excluded) - min(nodes_1_1$output_degree_self_excluded)) * 120

# -------------------------------------------------------------
# Highlight top exporting countries by out-degree
# -------------------------------------------------------------
top_nodes_1_1 <- nodes_1_1 %>%
  arrange(desc(output_degree_self_excluded)) %>%
  head(10)

# Update node font and color for top countries
nodes_1_1 <- nodes_1_1 %>%
  mutate(
    font.size = ifelse(id %in% top_nodes_1_1$id, 100, font.size),
    font.color = ifelse(id %in% top_nodes_1_1$id, "#f85959", "black"),
    color = ifelse(id %in% top_nodes_1_1$id, "crimson", "lightblue")
  )

# -------------------------------------------------------------
# Add percentage labels and tooltips
# -------------------------------------------------------------
nodes_1_1 <- nodes_1_1 %>%
  mutate(
    sum_values_self_exclu = sum(output_degree_self_excluded),
    sum_values_self_inclu = sum(output_degree_self_included),
    percentage_self_exclu = round((output_degree_self_excluded / sum_values_self_exclu) * 100, 2),
    percentage_self_inclu = round((output_degree_self_included / sum_values_self_inclu) * 100, 2)
  )

# Tooltip with degree info
nodes_1_1$title <- paste(
  "<b>Origin Country:</b>", nodes_1_1$label,
  "<br><b>Exported Contributions (Excluding Domestic):</b>",
  nodes_1_1$output_degree_self_excluded, 
  "(", nodes_1_1$percentage_self_exclu, "% of global contributions)",
  "<br><b>Total Contributions (Including Domestic):</b>",
  nodes_1_1$output_degree_self_included, 
  "(", nodes_1_1$percentage_self_inclu, "% of global contributions)"
)

# -------------------------------------------------------------
# Build and display the visNetwork graph
# -------------------------------------------------------------
g_1_1 <- visNetwork(nodes_1_1, edges_1_1) %>%
  visOptions(
    highlightNearest = list(
      enabled = TRUE,
      degree = 1,
      labelOnly = FALSE
    ),
    selectedBy = list(
      variable = "label",
      main = "Select a Country to Highlight",
      highlight = TRUE,
      style = "width: 210px; font-size: 14px;"
    )
  ) %>%
  visEdges(
    arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
    color = list(color = 'lightblue', highlight = 'green')
  ) %>%
  visNodes(size = nodes_1_1$size) %>%
  visPhysics(
    solver = "forceAtlas2Based",
    stabilization = FALSE
  ) %>%
  visInteraction(
    dragNodes = TRUE,
    dragView = TRUE,
    zoomView = TRUE,
    tooltipDelay = 50
  )

# Render the graph
g_1_1
