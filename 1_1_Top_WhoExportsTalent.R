
# --import prepared Data
source("0_DataToUse.R")

# Create a list of unique countries as nodes_1_1
nodes_1_1 <- data.frame(
  id = unique(c(df_research_contributions$Origin_Country, df_research_contributions$Research_Country)),
  label = unique(c(df_research_contributions$Origin_Country, df_research_contributions$Research_Country)),
  font.size = 50 #size of node labels
)

# Prepare edges_1_1 for visNetwork

# Summarize the data to get flows between countries
edges_1_1 <- df_research_contributions %>%
  group_by(Origin_Country, Research_Country) %>%
  summarise(flow_count = n(), .groups = "drop") %>%
  mutate(width = flow_count) %>%  # Set edge width based on flow count
  rename(from = Origin_Country, to = Research_Country) %>% 
 # Normalize edge widths to keep arrows proportional
  mutate(
    flow_count_scaled = (flow_count - min(flow_count)) / (max(flow_count) - min(flow_count)),  # Scale flows
    width = 1 + flow_count_scaled * 90 # Adjust edge thickness
  )
# --------------------highlight the nodes_1_1 size and color by their degree of centrality


g <- graph_from_data_frame(graph_initial_edges, directed = TRUE, vertices = nodes_1_1)
# ------------highligh top out degree
nodes_1_1$output_degree_self_excluded <- degree(g, mode = "out",loops = F) #add with and without selfloop
# nodes_1_1$in_degree= degree(g, mode = "in",loops = T) #add with and without selfloop
nodes_1_1$output_degree_self_included <- degree(g, mode = "out",loops = T) 
# `self_inclusive_degree`: Includes contributions where origin country matches research country.
# `self_exclusive_degree`: Excludes such contributions.

# Normalize degree centrality for node size scaling (min-max scaling)
nodes_1_1$size <- nodes_1_1$output_degree_self_excluded 
nodes_1_1$size <- 30 + (nodes_1_1$output_degree_self_excluded - min(nodes_1_1$output_degree_self_excluded)) / 
  (max(nodes_1_1$output_degree_self_excluded) - min(nodes_1_1$output_degree_self_excluded)) * 120  #In this case, the smallest node size will be 30, and the largest node size will be 150, making the nodes_1_1 noticeably larger.
# nodes_1_1$size <-50+ (nodes_1_1$degree_centrality - min(nodes_1_1$degree_centrality)) /
# (max(nodes_1_1$degree_centrality) - min(nodes_1_1$degree_centrality)) * 10

# 
# # Scale Node Size Proportional to out-Degree
# # Scaling in-degree to a range [10, 30] (or adjust as needed)
# min_size <- min(nodes_1_1$out_degree )
# max_size <- max(nodes_1_1$out_degree )
# nodes_1_1$size <- min_size + (nodes_1_1$out_degree - min(nodes_1_1$out_degree)) / 
#   (max(nodes_1_1$out_degree) - min(nodes_1_1$out_degree)) * (max_size - min_size)

# Highlight Top out-Degree Nodes (e.g., top 10 nodes_1_1)
top_nodes_1_1 <- nodes_1_1 %>%
  arrange(desc(output_degree_self_excluded)) %>%
  head(10)  
top_nodes_1_1$label


nodes_1_1 <- nodes_1_1 %>%
  mutate(
    font.size = ifelse(
      id %in% top_nodes_1_1$id,
      100,  # Bold font, larger size, and red color
      font.size      # Default font for other nodes_1_1
    ),
    font.color = ifelse(
      id %in% top_nodes_1_1$id,
      "#f85959",  # Bold font, larger size, and red color
      "black"      # Default font for other nodes_1_1
    )
  ) %>%
  mutate(sum_values_self_exclu=sum(output_degree_self_excluded),
         sum_values_self_inclu=sum(output_degree_self_included)
  ) %>% 
  mutate(percentage_self_exclu =  round((output_degree_self_excluded / sum_values_self_exclu) * 100, 2),
         percentage_self_inclu =  round((output_degree_self_included / sum_values_self_inclu) * 100, 2)
  )

# Assign a distinct color or larger size to the most important nodes_1_1
nodes_1_1$color <- ifelse(nodes_1_1$id %in% top_nodes_1_1$id, "crimson", "lightblue") 
nodes_1_1$title <- nodes_1_1$title <- paste(
  "<b>Origin Country:</b>", nodes_1_1$label,
  "<br><b>Exported Contributions (Excluding Domestic):</b>",
  nodes_1_1$output_degree_self_excluded, 
  "(", nodes_1_1$percentage_self_exclu, "% of global contributions)",
  "<br><b>Total Contributions (Including Domestic):</b>",
  nodes_1_1$output_degree_self_included, 
  "(", nodes_1_1$percentage_self_inclu, "% of global contributions)"
)

# Visualize the graph with adjusted arrow sizes
g_1_1 <- visNetwork(nodes_1_1, edges_1_1)%>%
  visOptions( highlightNearest = list(
    enabled = TRUE,        # Enable highlighting
    degree = 1,            # Highlight nodes and edges one degree away
     # hover = TRUE,          # Highlight on hover
    labelOnly = FALSE      # Highlight nodes and edges, not just labels
  ),
            
             # nodesIdSelection = TRUE
             selectedBy = list(
               variable = "label",
               main = "Select a Country to Highlight",
               highlight=TRUE,
               style = "width: 210px; font-size: 14px;"
             )) %>%
  visEdges(
    arrows = list(
      to = list(enabled = TRUE, scaleFactor = 0.5)  # Scale arrowheads to 50% size
    ),
    color = list(color = 'lightblue', 
                 highlight = 'green'
                 # hover = 'black'
                 )
    # scaling = list(min = 0.5, max = 2)  # Normalize edge width scaling
  ) %>%
  visNodes(
    size = nodes_1_1$size # Assign node size based on degree centrality
  )%>%
  visPhysics(
    solver = "forceAtlas2Based",  # Dynamic movement
    stabilization = F
     # repulsion = -2000
  ) %>%
  visInteraction(
    dragNodes = TRUE,
    dragView = TRUE,
    zoomView = TRUE,
    tooltipDelay = 50 # Quick tooltip response
  )
# %>%
#   # visConfigure(enabled = TRUE)  %>%
#   htmlwidgets::onRender("
#     function(el, x) {
#       var network = x;
#       network.fit();               // Ensure the graph fits in the canvas
#       network.moveTo({ scale: 0.8, position: {x: 0, y: 0} }); // Set zoom and position
#     }
#   ")

 g_1_1
 
# top_Flow_info_table_1_1 <- nodes_1_1 %>%
#    mutate(sum_values_self_exclu=sum(output_degree_self_excluded),
#            sum_values_self_inclu=sum(output_degree_self_included)
#           ) %>% 
#    mutate(percentage_self_exclu =  round((output_degree_self_excluded / sum_values_self_exclu) * 100, 2),
#           percentage_self_inclu =  round((output_degree_self_included / sum_values_self_inclu) * 100, 2)
#           )%>% 
#    arrange(desc(percentage_self_exclu))%>%
#     head(10)%>%
#    select(c(label,output_degree_self_included,
#             output_degree_self_excluded,sum_values_self_inclu,
#             sum_values_self_exclu,percentage_self_inclu,percentage_self_exclu))
#  
# 
# # %>%
# #   formatStyle(
# #     'self_link_and_top_10',  # Apply styles based on the entire row
# #     target = 'row',  # Style the entire row
# #     backgroundColor = styleEqual(
# #       c(1),  # Check for is_top_10 == 1
# #       c('#daebee')  # Light blue for top 10 rows
# #     ))
# 
# sum(top_Flow_info_table_1_1$percentage_self_exclu)
#  