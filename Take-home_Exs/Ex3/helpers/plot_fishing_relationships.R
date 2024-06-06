plot_fishing_relationships <- function(graph,
                                       # The network to plot
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
      family = STYLES$font_family,
      size = STYLES$node_label_size,
      color = STYLES$node_label_light
    ) +
    
    # Render edges. Use geom_edge fan so edges along the same path don't overlap
    geom_edge_fan(
      aes(color = subtype),
      strength = 0.5,
      arrow = STYLES$arrow_style,
      end_cap = circle(arrow_margin, "mm"),
      start_cap = circle(arrow_margin, "mm"),
      alpha = 0.8
    ) +
    scale_shape_manual(values = MAPPINGS$node_supertype_to_shape) +
    scale_color_manual(values = MAPPINGS$node_subtype_to_color) +
    scale_edge_color_manual(values = MAPPINGS$edge_relationship_subtype_to_color) +
    
    # Change legend names
    labs(shape = "Node Supertypes",
         color = "Node Subtypes",
         edge_color = "Edge Subtypes") +
    
    # Make sure the plot is not clipped
    scale_x_continuous(expand = expansion(mult = c(0.10, 0.10))) +
    scale_y_continuous(expand = expansion(mult = c(0.10, 0.10))) +
    
    # Style legend keys
    guides(
      shape = guide_legend(override.aes = list(size = 3, color = STYLES$primary_color), order = 1),
      color = guide_legend(override.aes = list(size = 3), order = 2),
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