
# --import prepared Data
source("0_DataToUse.R")
# --Note : here we relied on number of research contributions, not the percentage 
# of research contributions , because the number is more relevent : 
# e.g : if a country A has only 10 rsearch contrubutions and 10 of them is 
#  is self reseach ,it s not very relevant 

Data_to_use <- Data_WithCountryNamesInfo %>% 
  select(c(countryOrigin,continents,countryAffiliation ))%>% 
  mutate(var_self= ifelse(countryOrigin==countryAffiliation,countryAffiliation,continents))

Data_to_use <-Data_to_use[complete.cases(Data_to_use), ]

all_selfflows<- as.data.frame(table(Data_to_use$countryOrigin,Data_to_use$countryAffiliation)) %>%
  filter(as.character(Var1) == as.character(Var2)) %>%
  arrange(desc(Freq))
selfflows<- all_selfflows%>%
  head(10)

# Data_to_use <- Data_to_use[c(1:1000),]
# Assume df contains aggregated traffic between origin and research countries
Data_aggregated <- as.data.frame(table(Data_to_use$countryOrigin,Data_to_use$var_self))%>%
  filter(Freq>0)

selfflows$Var1

all_flows <- data.frame(
  source =  Data_aggregated$Var1,
  target = Data_aggregated$Var2,
  value = Data_aggregated$Freq
)



flows <- all_flows %>%
  filter(source %in% selfflows$Var1) %>% 
  # mutate(arrange_v=(as.character(source) == as.character(target))) %>% 
  # group_by(arrange_v)%>% 
  arrange(desc(value)) 
# %>% 
#   filter(value>0)

# Create a list of unique nodes_2_2 (countries)
nodes_2_2 <- data.frame(
  name = unique(c(flows$source, flows$target))
)
# # order countries based on 
# nodes_2_2 <- nodes_2_2 %>%
#   mutate(prio = match(name, selfflows$Var1, nomatch = Inf)) %>%
#   arrange(prio) %>%
#   select(-prio)


# Add numeric IDs for source and target
flows$source_id <- match(flows$source, nodes_2_2$name) - 1
flows$target_id <- match(flows$target, nodes_2_2$name) - 1


# -------------add percentage info
# Calculate total flow for each source
flows$total_from_source <- ave(flows$value, flows$source, FUN = sum)


# Create the Sankey diagram
sankey <- sankeyNetwork(
  Links = flows,
  Nodes = nodes_2_2,
  Source = "source_id",
  Target = "target_id",
  Value = "value",
  NodeID = "name",
  units = "Research Contributions",
  fontSize = 15,
  nodeWidth = 20,
  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")  # Optional color scheme
)

sankey2_1 <- htmlwidgets::onRender(
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
sankey_with_title <- htmltools::browsable(
  tags$div(
    style = "position: relative;",
    sankey2_1,  # The Sankey diagram
    tags$div(
      "Top 10 Countries by Domestic Research Contributions",
      style = "
        position: absolute;
        top: 80px;         /* Adjust vertically */
        right: 380px;       /* Adjust horizontally */
        font-size: 20px;   /* Font size for the title */
        font-weight: bold; /* Make the title bold */
        color: black;      /* Title color */
      "
    )
  )
)

# Display the Sankey diagram with the title
# sankey_with_title


# Create a summary table with percentage


# all_flows <- data.frame(
#   source =  Data_aggregated$Var1,
#   target = Data_aggregated$Var2,
#   value = Data_aggregated$Freq
# )

all_flows$total_from_source <- ave(all_flows$value, all_flows$source, FUN = sum)

flow_info_table <- all_flows %>%
  group_by(source)%>%
  mutate(percentage = (value / total_from_source) * 100)%>% 
  arrange(desc(percentage))  %>%
  summarise(
    Research_flows = paste("<b>",source,"</b>", "→", "<b>",target,"</b>", ":", value, "Research Contributions", "(", round(percentage, 2), "%)", collapse = "<br/>")
  ) %>%
  ungroup()%>%
  mutate(prio = match(source, all_selfflows$Var1, nomatch = Inf)) %>%
  arrange(prio) %>%
  select(-prio) %>% 
  rename("Origin Country"="source")



# Display the table as an interactive table
# Display the table with HTML enabled
dt_2_2 <- datatable(
  flow_info_table,
  escape = FALSE, # Disable HTML escaping
  options = list(pageLength = 5, autoWidth = TRUE)
)

# dt_2_2

# 
# # Create the Sankey diagram
# sankey <- sankeyNetwork(
#   Links = flows,
#   Nodes = nodes_2_2,
#   Source = "source_id",
#   Target = "target_id",
#   Value = "value",
#   NodeID = "name",
#   units = "People",
#   fontSize = 12,
#   nodeWidth = 30
# )
# 
# # Display the Sankey diagram
# sankey
#calculate :
sum_flows <- sum(all_flows$value)
sum(selfflows$Freq)
