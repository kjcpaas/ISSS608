plot_centrality <- function(graph,
                            # The network to plot
                            # Centrality measure, can be
                            # pagerank: for most powerful nodes
                            # betweeness: for power brokers
                            centrality,
                            # Layout options
                            layout = "nicely",
                            circular = FALSE,
                            # Texts
                            title = NULL,
                            subtitle = NULL,
                            caption = "Bigger and brighter nodes are **more influential**. *Hover on the nodes to see their full names*",
                            arrow_margin = 1.5,
                            # Seed
                            seed_num = DEFAULT_SEED) {
  set.seed(seed_num)
  
  if (!(centrality %in% c("pagerank", "betweenness"))) {
    stop("Only pagerank and betweenness centralities are relevant in our analysis")
  }
  
  centrality_col <- paste0(centrality, "_score")
  nodes <- graph %>% activate(nodes) %>% as.data.frame()
  score_boundary <- nodes[[centrality_col]] %>% quantile(probs = 0.75)
  
  g <- ggraph(graph, layout = layout, circular = circular) +
    # Render nodes
    geom_point_interactive(
      aes(
        x = x,
        y = y,
        data_id = name,
        tooltip = sprintf("%s<br/>(%s)[%0.5f]", name, subtype, .data[[centrality_col]]),
        color = subtype,
        # To show people as triangle, organizations as circle
        # See scale_shape_manual code below
        shape = supertype,
        # Get centrality measures froom a column
        size = .data[[centrality_col]],
        alpha = .data[[centrality_col]]
      )
    ) +
    geom_node_text(
      aes(label = alias),
      family = FONT_FAMILY,
      size = 2,
      # Nodes with low alpha have black text
      color = ifelse(nodes[[centrality_col]] < score_boundary, "black", "white")
    ) +
    
    # Render edges. Use geom_edge fan so edges along the same path don't overlap
    geom_edge_fan (
      aes(color = subtype),
      strength = 0.5,
      arrow = arrow(type = "closed", length = unit(0.02, "npc")),
      end_cap = circle(arrow_margin, "mm"),
      start_cap = circle(arrow_margin, "mm"),
      alpha = 0.8
    ) +
    # Person => triangle, Organization = Circle
    scale_shape_manual(values = c("Person" = 17, "Organization" = 16)) +
    
    # Color schemes
    # Colorblind pallettes from https://davidmathlogic.com/colorblind
    scale_color_manual(
      values = c(
        "Person" = "#44AA99",
        "CEO" = "#117733",
        "Company" = "#DDCC77",
        "FishingCompany" = "#332288",
        "LogisticsCompany" = "#88CCEE",
        "FinancialCompany" = "#AA4499",
        "NGO" = "#CC6677",
        "NewsCompany" = "#882255"
      )
    ) +
    
    # Color scheme for Edge subtype
    scale_edge_color_manual(
      values = c(
        "WorksFor" = "#D81B60",
        "Shareholdership" = "#1E88E5",
        "BeneficialOwnership" = "#004D40",
        "FamilyRelationship" = "#FFC107"
      )
    ) +
    
    # Centrality visualization
    scale_size_continuous(range = c(2, 10), guide = FALSE) +
    scale_alpha_continuous(range = c(0.5, 1), guide = FALSE) +
    
    # Change legend names
    labs(color = "Node Subtypes",
         shape = "Node Supertypes",
         edge_color = "Edge Subtypes") +
    
    # Make sure the plot is not clipped
    scale_x_continuous(expand = expand_scale(mult = c(0.10, 0.10))) +
    scale_y_continuous(expand = expand_scale(mult = c(0.10, 0.10))) +
    
    # Style legend keys
    guides(
      shape = guide_legend(override.aes = list(size = 2), order = 1),
      color = guide_legend(override.aes = list(size = 2), order = 2),
      edge_color = guide_legend(order = 3),
    ) +
    
    # Style graph
    unset_graph_style() +
    theme_graph(base_family = FONT_FAMILY, plot_margin = margin(0)) +
    plot_annotation(title = title,
                    subtitle = subtitle,
                    caption = caption) &
    theme(
      text = element_text(family = FONT_FAMILY),
      plot.title = element_text(face = "bold", size = unit(10, "pt")),
      plot.subtitle = element_markdown(color = "grey50", size = unit(8, "pt")),
      plot.caption = element_markdown(
        color = "grey50",
        size = unit(6, "pt"),
        hjust = 0
      ),
      legend.position = "right",
      legend.location = "plot",
      legend.title = element_markdown(size = unit(8, "pt"), face = "bold"),
      legend.text = element_markdown(size = unit(6, "pt"), vjust = 0.5),
      legend.justification = "center",
      legend.spacing = unit(0, "pt"),
      legend.key.spacing = unit(1, "pt"),
      legend.margin = margin(0),
      legend.box.margin = margin(-28, 0, 0, 0),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        size = 1
      )
    )
  
  # Add interactive elements
  tooltip_css = paste0(
    "background-color:black;color:white;font-family:",
    FONT_FAMILY,
    ";padding:4px;text-align:center;"
  )
  
  girafe(
    ggobj = g,
    width_svg = 6,
    height_svg = 6 * 0.618,
    options = list(opts_tooltip(css = tooltip_css))
  )
}