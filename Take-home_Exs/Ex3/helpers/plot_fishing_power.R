plot_fishing_relationships <- function(# The network to plot
  graph,
  # Layout options
  layout = "nicely",
  circular = FALSE,
  # Texts
  title = NULL,
  subtitle = NULL,
  caption = "Hover on the nodes to see their full names",
  # Plot styling
  node_size = 7.5,
  arrow_margin = 3.2,
  edge_thickness = 0.15,
  # Seed
  seed_num = DEFAULT_SEED) {
  set.seed(seed_num)
  
  edges <- graph %>% activate(edges) %>% as.data.frame()
  max_weight <- edges$weight %>% max()
  min_weight <- edges$weight %>% min()
  g <- ggraph(graph, layout = layout, circular = circular) +
    # Render nodes
    geom_point_interactive(
      aes(
        x = x,
        y = y,
        data_id = name,
        tooltip = sprintf("%s<br/>(%s)", name, subtype),
        color = subtype,
        # To show people as triangle, organizations as circle
        # See scale_shape_manual code below
        shape = supertype,
      ),
      size = node_size
    ) +
    geom_node_text(
      aes(label = alias),
      family = FONT_FAMILY,
      size = 2,
      color = 'white'
    ) +
    
    # Render edges. Use geom_edge fan so edges along the same path don't overlap
    geom_edge_fan (
      aes(color = subtype, edge_width = weight),
      strength = 0.5,
      arrow = arrow(type = "closed", length = unit(0.02, "npc")),
      end_cap = circle(arrow_margin, "mm"),
      start_cap = circle(arrow_margin, "mm"),
      alpha = 0.8,
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
        "HasShareholder" = "#1E88E5",
        "OwnedBy" = "#004D40",
        "FamilyRelationship" = "#FFC107"
      )
    ) +
    
    # Make sure edge widths are consistent across diff graphs
    scale_edge_width(
      range = c(min_weight * edge_thickness, max_weight * edge_thickness),
      guide = "none"
    ) +
    
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
      edges = guide_legend(order = 3)
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