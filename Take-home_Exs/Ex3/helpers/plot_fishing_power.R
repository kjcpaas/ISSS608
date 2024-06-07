plot_fishing_power <- function(graph,
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
                               edge_thickness = STYLES$base_edge_thickness,
                               # Seed
                               seed_num = CONFIGS$default_seed) {
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
        fill = subtype,
        # To show people as triangle, organizations as circle
        # See scale_shape_manual code below
        shape = supertype,
      ),
      size = node_size,
      color = STYLES$node_border_color
    ) +
    geom_node_text(
      aes(label = alias),
      family = STYLES$font_family,
      size = STYLES$node_label_size,
      color = STYLES$node_label_light
    ) +
    
    # Render edges. Use geom_edge fan so edges along the same path don't overlap
    geom_edge_fan(
      aes(color = subtype, edge_width = weight),
      strength = 0.5,
      arrow = STYLES$arrow_style,
      end_cap = circle(arrow_margin, "mm"),
      start_cap = circle(arrow_margin, "mm"),
      alpha = 0.8
    ) +
    scale_shape_manual(values = MAPPINGS$node_supertype_to_shape) +
    scale_fill_manual(values = MAPPINGS$node_subtype_to_color) +
    scale_edge_color_manual(values = MAPPINGS$edge_power_subtype_to_color) +
    
    # Make sure edge widths are consistent across diff graphs
    scale_edge_width(
      range = c(min_weight * edge_thickness, max_weight * edge_thickness),
      guide = "none"
    ) +
    
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
        override.aes = list(
          size = 3,
          color = STYLES$node_border_color,
          fill = STYLES$primary_color
        ),
        order = 1
      ),
      fill = guide_legend(
        override.aes = list(size = 4, shape = 22, color = NA),
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