plot_fishing_relationships <- function(graph,
                                       # Name of nodes to emphasize
                                       emphasize_nodes = c(),
                                       # Date where to get the snapshot
                                       datestring = NULL,
                                       # Layout options
                                       layout = "nicely",
                                       circular = FALSE,
                                       # Texts
                                       title = NULL,
                                       subtitle = NULL,
                                       caption = STYLES$default_caption,
                                       # Plot styling
                                       node_size = STYLES$node_size,
                                       arrow_margin = STYLES$arrow_margin,
                                       # Seed
                                       seed_num = CONFIGS$default_seed) {
  set.seed(seed_num)
  
  date <- NULL
  if(!is.null(datestring)) { date <- as_date(datestring) }
  nodes <- as_data_frame(graph, what = "vertices")
  
  g <- ggraph(graph, layout = layout, circular = circular) +
    # Render nodes
    geom_point_interactive(
      aes(
        x = x,
        y = y,
        data_id = name,
        tooltip = sprintf("%s<br/>(%s)", name, subtype),
        fill = subtype,
        # To show people as triangle, organizations as circle
        # See scale_shape_manual code below
        shape = supertype,
      ),
      size = node_size,
      # Thicken border if emphasized
      color = ifelse(
        nodes$name %in% emphasize_nodes,
        STYLES$node_emphasized_border_color,
        STYLES$node_border_color
      ),
      stroke = ifelse(
        nodes$name %in% emphasize_nodes,
        STYLES$node_emphasized_border_stroke,
        STYLES$node_border_stroke
      ),
    ) +
    geom_node_text(
      aes(label = alias),
      family = STYLES$font_family,
      size = STYLES$node_label_size,
      color = STYLES$node_label_light,
      fontface = ifelse(nodes$name %in% emphasize_nodes, "bold", "plain"),
    ) +
    
    # Render edges. Use geom_edge fan so edges along the same path don't overlap
    geom_edge_fan(
      aes(
        color = subtype,
        # Will identify if the edge is active at this date, if not do not display
        # Ideally this should be in a function but I can't figure out how to make it work inside aes
        # Logic is same as is extract_network_snapshot.R
        filter = ifelse(is.null(date) | is.na(start_date), TRUE,
          ifelse(start_date <= date & (is.na(end_date) | end_date > date),
                 TRUE,
                 FALSE
                 )
        )
      ),
      strength = 0.5,
      arrow = STYLES$arrow_style,
      end_cap = circle(arrow_margin, "mm"),
      start_cap = circle(arrow_margin, "mm"),
      alpha = 0.8
    ) +
    scale_shape_manual(values = MAPPINGS$node_supertype_to_shape) +
    scale_fill_manual(values = MAPPINGS$node_subtype_to_color) +
    scale_edge_color_manual(values = MAPPINGS$edge_relationship_subtype_to_color) +
    
    # Change legend names
    labs(shape = "Node Supertypes",
         fill = "Node Subtypes",
         edge_color = "Edge Subtypes") +
    
    # Make sure the plot is not clipped
    scale_x_continuous(expand = expansion(mult = c(0.10, 0.10))) +
    scale_y_continuous(expand = expansion(mult = c(0.10, 0.10))) +
    
    # Style legend keys
    guides(
      shape = guide_legend(
        override.aes = list(size = 3, fill = STYLES$primary_color),
        order = 1
      ),
      fill = guide_legend(
        override.aes = list(
          size = 4,
          shape = 22,
          color = NA
        ),
        order = 2
      ),
      edge_color = guide_legend(order = 3),
    ) +
    
    # Style graph
    unset_graph_style() +
    theme_graph(base_family = STYLES$font_family,
                plot_margin = margin(0)) +
    
    plot_annotation(title = title,
                    subtitle = subtitle,
                    caption = caption) &
    COMMON_THEME
  
  girafe(
    ggobj = g,
    width_svg = STYLES$svg_width,
    height_svg = STYLES$svg_height,
    options = list(opts_tooltip(css = STYLES$tooltip_css))
  )
}