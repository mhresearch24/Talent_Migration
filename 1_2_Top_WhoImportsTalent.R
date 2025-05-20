
# --import prepared Data
source("0_DataToUse.R")

# Create a list of unique countries as nodes_1_2
nodes_1_2 <- data.frame(
  id = unique(c(df_research_contributions$Origin_Country, df_research_contributions$Research_Country)),
  label = unique(c(df_research_contributions$Origin_Country, df_research_contributions$Research_Country)),
  font.size = 50 #size of node name
)

# Prepare edges_1_2 for visNetwork

# Summarize the data to get flows between countries
edges_1_2 <- df_research_contributions %>%
  group_by(Origin_Country, Research_Country) %>%
  summarise(flow_count = n(), .groups = "drop") %>%
  mutate(width = flow_count) %>%  # Set edge width based on flow count
  rename(from = Origin_Country, to = Research_Country) %>% 
  # Normalize edge widths to keep arrows proportional
  mutate(
    flow_count_scaled = (flow_count - min(flow_count)) / (max(flow_count) - min(flow_count)),  # Scale flows
    width = 1 + flow_count_scaled * 90 # Adjust edge thickness
  )
# --------------------highlight the nodes_1_2 size and color by their degree of centrality


g <- graph_from_data_frame(graph_initial_edges, directed = TRUE, vertices = nodes_1_2)
# ------------highligh top out degree
nodes_1_2$input_degree_self_excluded <- degree(g, mode = "in",loops = F) #add with and without selfloop
# nodes_1_2$in_degree= degree(g, mode = "in",loops = T) #add with and without selfloop
nodes_1_2$input_degree_self_included <- degree(g, mode = "in",loops = T) 
# `self_inclusive_degree`: Includes contributions where origin country matches research country.
# `self_exclusive_degree`: Excludes such contributions.

# Normalize degree centrality for node size scaling (min-max scaling)
nodes_1_2$size <- nodes_1_2$input_degree_self_excluded 
nodes_1_2$size <- 30 + (nodes_1_2$input_degree_self_excluded - min(nodes_1_2$input_degree_self_excluded)) / 
  (max(nodes_1_2$input_degree_self_excluded) - min(nodes_1_2$input_degree_self_excluded)) * 120  #In this case, the smallest node size will be 30, and the largest node size will be 150, making the nodes_1_2 noticeably larger.
# nodes_1_2$size <-50+ (nodes_1_2$degree_centrality - min(nodes_1_2$degree_centrality)) /
# (max(nodes_1_2$degree_centrality) - min(nodes_1_2$degree_centrality)) * 10

# 
# # Scale Node Size Proportional to out-Degree
# # Scaling in-degree to a range [10, 30] (or adjust as needed)
# min_size <- min(nodes_1_2$out_degree )
# max_size <- max(nodes_1_2$out_degree )
# nodes_1_2$size <- min_size + (nodes_1_2$out_degree - min(nodes_1_2$out_degree)) / 
#   (max(nodes_1_2$out_degree) - min(nodes_1_2$out_degree)) * (max_size - min_size)
nodes_1_2 <- nodes_1_2%>%
  mutate(sum_values_self_exclu=sum(input_degree_self_excluded),
         sum_values_self_inclu=sum(input_degree_self_included)
  ) %>% 
  mutate(percentage_self_exclu =  round((input_degree_self_excluded / sum_values_self_exclu) * 100, 2),
         percentage_self_inclu =  round((input_degree_self_included / sum_values_self_inclu) * 100, 2)
  )
# Highlight Top out-Degree Nodes (e.g., top 10 nodes_1_2)
top_nodes_1_2 <- nodes_1_2 %>%
  arrange(desc(input_degree_self_excluded)) %>%
  head(10)  

# Assign a distinct color or larger size to the most important nodes_1_2
nodes_1_2$color <- ifelse(nodes_1_2$id %in% top_nodes_1_2$id, "#259073", "lightblue") 

# customise font of top nodes_1_2
# Update the font attribute for top nodes_1_2
nodes_1_2 <- nodes_1_2 %>%
  mutate(
    font.size = ifelse(
      id %in% top_nodes_1_2$id,
      100,  # Bold font, larger size, and red color
      font.size      # Default font for other nodes_1_2
    ),
    font.color = ifelse(
      id %in% top_nodes_1_2$id,
       "#97124b",
        # "lightseagreen",  # Bold font, larger size, and red color
      "black"      # Default font for other nodes_1_2
    )
  )


nodes_1_2$title <- paste(
  "<b>Research Hub:</b>", nodes_1_2$label,
  "<br><b>Total Imported Contributions (Excluding Domestic):</b>", nodes_1_2$input_degree_self_excluded, 
  "(", nodes_1_2$percentage_self_exclu, "% of total imported contributions)",
  "<br><b>Total Imported Contributions (Including Domestic):</b>", nodes_1_2$input_degree_self_included, 
  "(", nodes_1_2$percentage_self_inclu, "% of total imported contributions)"
)



# 
# visOptions( highlightNearest = list(
#   enabled = TRUE,        # Enable highlighting
#   degree = 1,            # Highlight nodes and edges one degree away
#   hover = TRUE,          # Highlight on hover
#   labelOnly = FALSE      # Highlight nodes and edges, not just labels
# ),



# Visualize the graph with adjusted arrow sizes
g_1_2 <- visNetwork(nodes_1_2, edges_1_2)%>%
  visOptions(highlightNearest = TRUE, 
             # nodesIdSelection = TRUE
             selectedBy = list(
               variable = "label",
               main = "Select a Country to Highlight",
               highlight=TRUE,
               style = "width: 210px; font-size: 14px;"
             )
             
             ) %>%
  visEdges(
    arrows = list(
      to = list(enabled = TRUE, scaleFactor = 0.5)  # Scale arrowheads to 50% size
    ),
    color = list(color = 'lightblue', highlight = 'green')
    # scaling = list(min = 0.5, max = 2)  # Normalize edge width scaling
  ) %>%
  visNodes(
    size = nodes_1_2$size # Assign node size based on degree centrality
  )%>%
  visPhysics(
    # enabled = FALSE,
    solver = "forceAtlas2Based",  # Dynamic movement
    stabilization = F,
    repulsion = -4000
  ) %>%
  visInteraction(
    dragNodes = T,
    dragView = T,
    zoomView = TRUE,
    tooltipDelay = 50 # Quick tooltip response
  )
g_1_2

top_nodes_1_2$label
sum(top_nodes_1_2$percentage_self_inclu)
# %>%
#   visConfigure(enabled = TRUE) 

# %>%
#   htmlwidgets::onRender("
#     function(el, x) {
#       var network = x;
#       network.fit();               // Ensure the graph fits in the canvas
#       network.moveTo({ scale: 0.8, position: {x: 0, y: 0} }); // Set zoom and position
#     }
#   ")

