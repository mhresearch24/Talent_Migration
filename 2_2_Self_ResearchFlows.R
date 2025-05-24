# ===============================
# Script Overview:
# ===============================
# This script analyzes self-research flows where the origin country of the researcher
# matches the country of affiliation. It produces:
# 
# 1) A Sankey diagram:
#    - Top 10 countries with the highest self-research contributions
#    - Shows how these self-research flows compare to flows toward other continents
# 
# 2) An interactive data table:
#    - Details of all countries and their research flow distributions
#    - Ordered based on top self-research contributors
#
# Note: We focus on the **number** of research contributions instead of **percentages**,
# because raw counts are more relevant for insights. Percentages are still shown in the output.

# ===============================
# 1. Load Prepared Data
# ===============================
source("0_DataToUse.R")  # Assumes this file prepares Data_WithCountryNamesInfo

# ===============================
# 2. Prepare Dataset for Analysis
# ===============================
# Select relevant columns and create a new variable 'var_self':
# - If origin == affiliation, it's a self-flow (set var_self = country name)
# - Else, group by affiliation's continent (to track non-self flows)

Data_to_use <- Data_WithCountryNamesInfo %>% 
  select(c(countryOrigin, continents, countryAffiliation)) %>%
  mutate(var_self = ifelse(countryOrigin == countryAffiliation, countryAffiliation, continents))

# Remove incomplete rows
Data_to_use <- Data_to_use[complete.cases(Data_to_use), ]

# ===============================
# 3. Identify Top Self-Flows
# ===============================
# Count how many times origin == affiliation for each country (self-contributions)
all_selfflows <- as.data.frame(table(Data_to_use$countryOrigin, Data_to_use$countryAffiliation)) %>%
  filter(as.character(Var1) == as.character(Var2)) %>%
  arrange(desc(Freq))

# Select the top 10 countries with the highest self-research contributions
selfflows <- all_selfflows %>% head(10)

# ===============================
# 4. Aggregate Flows for All Countries
# ===============================
# Aggregate flows: count how often a research flow starts in origin and flows to either
# itself (if self-flow) or to a continent (if not self)
Data_aggregated <- as.data.frame(table(Data_to_use$countryOrigin, Data_to_use$var_self)) %>%
  filter(Freq > 0)

# Rename for clarity
all_flows <- data.frame(
  source = Data_aggregated$Var1,
  target = Data_aggregated$Var2,
  value  = Data_aggregated$Freq
)

# Filter only flows from the top 10 self-contributors
flows <- all_flows %>%
  filter(source %in% selfflows$Var1) %>%
  arrange(desc(value))

# ===============================
# 5. Build Sankey Diagram
# ===============================
# Create unique list of nodes (countries and continents)
nodes_2_2 <- data.frame(
  name = unique(c(flows$source, flows$target))
)

# Assign numeric IDs for Sankey (starts at 0)
flows$source_id <- match(flows$source, nodes_2_2$name) - 1
flows$target_id <- match(flows$target, nodes_2_2$name) - 1

# Compute total outgoing flows from each source (used for percentages)
flows$total_from_source <- ave(flows$value, flows$source, FUN = sum)

# Create basic Sankey diagram
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
  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")
)

# Enhance the Sankey diagram: Add tooltips with % contribution from each source
sankey2_1 <- htmlwidgets::onRender(
  sankey,
  "
  function(el) {
    var links = d3.select(el).selectAll('.link');
    links.select('title').text(function(d) {
      var sourceTotal = d.source.sourceLinks.reduce((acc, cur) => acc + cur.value, 0);
      var percentage = ((d.value / sourceTotal) * 100).toFixed(2);
      return d.source.name + ' → ' + d.target.name + ': ' + d.value + ' Research Contributions (' + percentage + '% of total flow from ' + d.source.name + ')';
    });
  }
  "
)

# Add a custom title to the Sankey diagram
sankey_with_title <- htmltools::browsable(
  tags$div(
    style = "position: relative;",
    sankey2_1,
    tags$div(
      "Top 10 Countries by Domestic Research Contributions",
      style = "
        position: absolute;
        top: 80px;
        right: 380px;
        font-size: 20px;
        font-weight: bold;
        color: black;
      "
    )
  )
)

# Display Sankey
sankey_with_title

# ===============================
# 6. Generate Detailed Flow Table
# ===============================
# Compute total flow from each source 
all_flows$total_from_source <- ave(all_flows$value, all_flows$source, FUN = sum)

# Create a formatted table with:
# - Research flow direction
# - Value
# - Percentage of total source flow
# - Sorted by top self-contributors
flow_info_table <- all_flows %>%
  group_by(source) %>%
  mutate(percentage = (value / total_from_source) * 100) %>%
  arrange(desc(percentage)) %>%
  summarise(
    Research_flows = paste(
      "<b>", source, "</b>", "→", "<b>", target, "</b>",
      ":", value, "Research Contributions",
      "(", round(percentage, 2), "%)", collapse = "<br/>"
    )
  ) %>%
  ungroup() %>%
  mutate(prio = match(source, all_selfflows$Var1, nomatch = Inf)) %>%
  arrange(prio) %>%
  select(-prio) %>%
  rename("Origin Country" = "source")

# ===============================
# 7. Display Interactive Data Table
# ===============================
dt_2_2 <- datatable(
  flow_info_table,
  escape = FALSE,  # Allows rendering of HTML in the table
  options = list(pageLength = 5, autoWidth = TRUE)
)

# Display the datatable
dt_2_2
