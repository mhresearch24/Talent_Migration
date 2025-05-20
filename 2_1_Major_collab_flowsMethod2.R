
# --import prepared Data
source("0_DataToUse.R")

# Summarize the data to get flows between countries
edges_2_1 <- df_research_contributions %>%
  group_by(Origin_Country, Research_Country) %>%
  summarise(flow_count = n(), .groups = "drop")

all_flows_2_1 <- data.frame(
  source =  edges_2_1$Origin_Country,
  target = edges_2_1$Research_Country,
  value = edges_2_1$flow_count
)%>% 
  arrange(desc(value)) 

top_flows_2_1_1 <- all_flows_2_1 %>%
  filter(source != target) %>% 
  # mutate(arrange_v=(as.character(source) == as.character(target))) %>% 
  # group_by(arrange_v)%>% 
  arrange(desc(value)) %>% 
  head(10)


top_flows_2_1 <- all_flows_2_1 %>% 
  filter(source%in%top_flows_2_1_1$source) %>% 
  mutate(target=ifelse(source==target,
                       "Self Flow (origin is same as the research country)",
    
    ifelse(target%in%top_flows_2_1_1$target,
                       target,
                       "Rest of Countries")
         
         
         ) )%>% 
  group_by(source,target) %>% 
  mutate(value=sum(value)) %>% 
  ungroup() %>% 
  distinct()
  
  
# Create a list of unique nodes (countries)
nodes <- data.frame(
  name = unique(c(top_flows_2_1$source, top_flows_2_1$target))
)
# # # order countries based on 
# nodes <- nodes %>%
#   mutate(prio = match(name, top_flows_2_1_1$source, nomatch = Inf)) %>%
#   arrange(prio) %>%
#   select(-prio)


# Add numeric IDs for source and target
top_flows_2_1$source_id <- match(top_flows_2_1$source, nodes$name) - 1
top_flows_2_1$target_id <- match(top_flows_2_1$target, nodes$name) - 1


# -------------add percentage info
# Calculate total flow for each source
top_flows_2_1$total_from_source <- ave(top_flows_2_1$value, top_flows_2_1$source, FUN = sum)


# Create the Sankey diagram
sankey <- sankeyNetwork(
  Links = top_flows_2_1,
  Nodes = nodes,
  Source = "source_id",
  Target = "target_id",
  Value = "value",
  NodeID = "name",
  units = "Research Contributions",
  fontSize = 12,
  nodeWidth = 20,
  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")  # Optional color scheme
)
sankey <- htmlwidgets::onRender(
  sankey,
  "
  function(el) {
    // Get all links from the Sankey chart
    var links = d3.select(el).selectAll('.link');

    // Calculate percentage based on the total flow from the source
    links.select('title').text(function(d) {
      var sourceTotal = d.source.sourceLinks.reduce((acc, cur) => acc + cur.value, 0);
      var percentage = ((d.value / sourceTotal) * 100).toFixed(2);
      return d.source.name + ' → ' + d.target.name + ': ' + d.value + ' Research Contributions (' + percentage + '% of total flow from ' + d.source.name + ')';
    });
  }
  "
) 

# Display the Sankey diagram
sankey_with_title <- browsable(
  tags$div(
    style = "position: relative;",
    sankey,  # The Sankey diagram
    tags$div(
      "Top 10	Major collaboration flows between countries",
      style = "
        position: absolute;
        top: 10px;         /* Adjust vertically */
        right: 500px;       /* Adjust horizontally */
        font-size: 20px;   /* Font size for the title */
        font-weight: bold; /* Make the title bold */
        color: black;      /* Title color */
      "
    )
  )
)

# Display the Sankey diagram with the title
# sankey_with_title


# ---------------table summary


# all_flows_2_1$total_from_source <- ave(all_flows_2_1$value, all_flows_2_1$source, FUN = sum)

Flow_info_table_2_1 <- all_flows_2_1 %>%
  mutate(sum_flows=sum(value)) %>% 
  mutate(percentage =  round((value / sum_flows) * 100, 2))%>% 
  arrange(desc(percentage))  %>%
  group_by(source,target) %>% 
  mutate(
    Research_flows = paste("<b>",source,"</b>", "→", "<b>",target,"</b>", ":", value, "Research Contributions", "(", percentage, "% of all Research Contributions)", collapse = "<br/>")
  ) %>% 
  select(source, target,Research_flows,percentage) %>% 
  rename("Authors Origin"="source",
         "Research Destination" = "target") %>% 
  ungroup() %>%
  mutate(
    row_number = row_number()) %>% 
  mutate(
    is_top_10 = ifelse(row_number <= 15, 1, 0),
    is_self_link = ifelse(`Authors Origin` == `Research Destination`, 1, 0),
    self_link_and_top_10 = is_top_10&(!is_self_link)
  )

# Render the datatable with conditional formatting

# Render the datatable with correct conditional formatting
Dt_2_1 <- datatable(Flow_info_table_2_1, escape = FALSE,options = list(pageLength = 15,
                                                             columnDefs = list(
                                                               list(targets = c("row_number","is_top_10", "is_self_link","self_link_and_top_10"), visible = FALSE)  # Hide columns by index (0-based)
                                                             ),
                                                             autoWidth = TRUE)) %>%
  formatStyle(
    'self_link_and_top_10',  # Apply styles based on the entire row
    target = 'row',  # Style the entire row
    backgroundColor = styleEqual(
      c(1),  # Check for is_top_10 == 1
      c('#daebee')  # Light blue for top 10 rows
    )) 
# Dt_2_1

# interpret <- unique(c(top_flows_2_1$source,top_flows_2_1$target))
# 
# sum(head(Flow_info_table_2_1$percentage,15))
# 
# head_sources <- head(Flow_info_table_2_1$`Authors Origin`,10)
#  head_target <- head(Flow_info_table_2_1$`Research Destination`,10)
# #   
# tt <- Flow_info_table_2_1 %>% 
#   filter(`Authors Origin`%in%c(top_flows_2_1$source,top_flows_2_1$target)|`Research Destination`%in%c(top_flows_2_1$source,top_flows_2_1$target))
# 
# unique(as.character(c(head_sources,head_target)))  
# sum(tt$percentage)
