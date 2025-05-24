# =============================================================================
# Script: major_collaboration_flows.R
# Description:
#   This script generates two key outputs:
#     1. A Sankey diagram highlighting the top international research collaboration flows
#     2. A data table summarizing all significant flows between origin and research countries
#   The focus is on visualizing and interpreting the most prominent research migration patterns.
# =============================================================================

# -----------------------------------------------------------------------------
# Load processed data (preprocessed in '0_DataToUse.R')
# This loads the dataframe 'df_research_contributions'
# -----------------------------------------------------------------------------
source("0_DataToUse.R")

# -----------------------------------------------------------------------------
# Step 1: Summarize data to get collaboration flows between countries
# -----------------------------------------------------------------------------
edges_2_1 <- df_research_contributions %>%
  group_by(Origin_Country, Research_Country) %>%
  summarise(flow_count = n(), .groups = "drop")

all_flows_2_1 <- data.frame(
  source = edges_2_1$Origin_Country,
  target = edges_2_1$Research_Country,
  value = edges_2_1$flow_count
) %>%
  arrange(desc(value))

# -----------------------------------------------------------------------------
# Step 2: Identify top 10 international flows (excluding domestic flows)
# -----------------------------------------------------------------------------
top_flows_2_1_1 <- all_flows_2_1 %>%
  filter(source != target) %>%
  arrange(desc(value)) %>%
  head(10)

# -----------------------------------------------------------------------------
# Step 3: Simplify remaining flows
#   - Label domestic flows as "Self Flow"
#   - Group non-top target countries into "Rest of Countries"
# -----------------------------------------------------------------------------
top_flows_2_1 <- all_flows_2_1 %>%
  filter(source %in% top_flows_2_1_1$source) %>%
  mutate(target = ifelse(source == target,
                         "Self Flow (origin is same as the research country)",
                         ifelse(target %in% top_flows_2_1_1$target,
                                target,
                                "Rest of Countries"))) %>%
  group_by(source, target) %>%
  mutate(value = sum(value)) %>%
  ungroup() %>%
  distinct()

# -----------------------------------------------------------------------------
# Step 4: Prepare nodes and IDs for the Sankey diagram
# -----------------------------------------------------------------------------
nodes <- data.frame(name = unique(c(top_flows_2_1$source, top_flows_2_1$target)))

top_flows_2_1$source_id <- match(top_flows_2_1$source, nodes$name) - 1
top_flows_2_1$target_id <- match(top_flows_2_1$target, nodes$name) - 1

# Add total outgoing flow per source for percentage calculation
top_flows_2_1$total_from_source <- ave(top_flows_2_1$value, top_flows_2_1$source, FUN = sum)

# -----------------------------------------------------------------------------
# Step 5: Create the Sankey diagram using networkD3
# -----------------------------------------------------------------------------
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
  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")
)

# Enhance tooltips with percentage information
sankey <- htmlwidgets::onRender(
  sankey,
  "
  function(el) {
    var links = d3.select(el).selectAll('.link');
    links.select('title').text(function(d) {
      var sourceTotal = d.source.sourceLinks.reduce((acc, cur) => acc + cur.value, 0);
      var percentage = ((d.value / sourceTotal) * 100).toFixed(2);
      return d.source.name + ' → ' + d.target.name + ': ' + d.value +
             ' Research Contributions (' + percentage + '% of total flow from ' +
             d.source.name + ')';
    });
  }
  "
)

# -----------------------------------------------------------------------------
# Step 6: Add title and style to the Sankey diagram
# -----------------------------------------------------------------------------
sankey_with_title <- browsable(
  tags$div(
    style = "position: relative;",
    sankey,
    tags$div(
      "Top 10 Major Collaboration Flows Between Countries",
      style = "
        position: absolute;
        top: 10px;
        right: 500px;
        font-size: 20px;
        font-weight: bold;
        color: black;
      "
    )
  )
)

# Display the diagram
sankey_with_title

# -----------------------------------------------------------------------------
# Step 7: Generate data table with flow details
# -----------------------------------------------------------------------------
Flow_info_table_2_1 <- all_flows_2_1 %>%
  mutate(
    sum_flows = sum(value),
    percentage = round((value / sum_flows) * 100, 2)
  ) %>%
  arrange(desc(percentage)) %>%
  group_by(source, target) %>%
  mutate(
    Research_flows = paste0(
      "<b>", source, "</b> → <b>", target, "</b>: ", value,
      " Research Contributions (", percentage, "% of all Research Contributions)"
    )
  ) %>%
  select(source, target, Research_flows, percentage) %>%
  rename(
    "Authors Origin" = "source",
    "Research Destination" = "target"
  ) %>%
  ungroup() %>%
  mutate(
    row_number = row_number(),
    is_top_10 = ifelse(row_number <= 15, 1, 0),
    is_self_link = ifelse(`Authors Origin` == `Research Destination`, 1, 0),
    self_link_and_top_10 = is_top_10 & (!is_self_link)
  )

# -----------------------------------------------------------------------------
# Step 8: Render interactive datatable with formatting
# -----------------------------------------------------------------------------

Dt_2_1 <- datatable(
  Flow_info_table_2_1,
  escape = FALSE,
  options =
    list(
      pageLength = 15,
      columnDefs = list(list(
        targets = c(
          "row_number",
          "is_top_10",
          "is_self_link",
          "self_link_and_top_10"
        ),
        visible = FALSE
      )),
      autoWidth = TRUE
    )
) %>%
  formatStyle('self_link_and_top_10',
              target = 'row',
              backgroundColor = styleEqual(c(1), c('#daebee')))


# Display the datatable
Dt_2_1
