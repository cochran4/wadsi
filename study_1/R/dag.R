library(ggplot2)
library(dplyr)
library(grid)
library(cowplot)


# ================================
# Directed graph
# ================================

# --- Node coordinates ---
nodes <- data.frame(
  name = c("X", "Y", "Z", "S"),
  x = c(-1, 0, -1, 1.2),
  y = c(.7, 0, -.7, 0)
)

# --- Directed edges only ---
edges <- data.frame(
  from = c("X","Z","Y"),
  to   = c("Y","Y","S")
)

# --- Merge coordinates for directed edges ---
edges <- merge(edges, nodes, by.x = "from", by.y = "name")
edges <- merge(edges, nodes, by.x = "to", by.y = "name",
               suffixes = c("_from", "_to"))

# --- Shorten arrows so they stop at circle edge ---
node_radius <- 0.23
edges <- edges %>%
  rowwise() %>%
  mutate(
    dx = x_to - x_from,
    dy = y_to - y_from,
    dist = sqrt(dx^2 + dy^2),
    shrink = node_radius / dist,
    x_from_adj = x_from + dx * shrink,
    y_from_adj = y_from + dy * shrink,
    x_to_adj   = x_to   - dx * shrink,
    y_to_adj   = y_to   - dy * shrink
  )

# --- Mixed edge between X and Z: curved, no arrowheads ---
xz_edge <- data.frame(
  x    = -1,
  y    =  0.7 - node_radius + .05,
  xend = -1,
  yend = -0.7 + node_radius -.05
)

# --- Plot ---
p_dag <- ggplot() +
  # directed arrows
  geom_segment(
    data = edges,
    aes(x = x_from_adj, y = y_from_adj,
        xend = x_to_adj, yend = y_to_adj),
    arrow = grid::arrow(length = unit(0.18, "inches"), type = "closed"),
    linewidth = 1.0, color = "black",
    lineend = "butt"
  ) +
  # curved X-Z connection with no heads
  geom_curve(
    data = xz_edge,
    aes(x = x-.2, y = y+.1, xend = xend-.2, yend = yend-.1),
    curvature = 0.4,   # bends left
    linewidth = 1.0,
    color = "black",
    lineend = "round"
  ) +
  # node circles
  geom_point(
    data = nodes,
    aes(x = x, y = y),
    fill = "#009E73", shape = 21, size = 25,
    color = "black", stroke = 1.0
  ) +
  # node labels
  geom_text(
    data = nodes,
    aes(x = x, y = y, label = name),
    color = "white", fontface = "bold", size = 7
  ) +
  coord_equal(xlim = c(-1.4, 1.4), ylim = c(-.7, .9), clip = "off") +
  theme_void(base_size = 14) +
  theme(plot.margin = margin(20, 15, 15, 15))


# ================================
# Single world intervention graph
# ================================

# --- Node coordinates ---
nodes <- data.frame(
  name  = c("X_left", "X_right", "Y", "Z", "S"),
  label = c("X", "x", "Y(x)", "Z", "S(x)"),
  x     = c(-1, -0.7, 0, -1, 1.2),
  y     = c(0.7, 0.7, 0, -0.7, 0)
)

# --- Directed edges only ---
# Remove Z -> X_left and X_left -> Z; draw that connection separately as curved/no-head
edges <- data.frame(
  from = c("X_right", "Y", "Z"),
  to   = c("Y", "S", "Y")
)

# --- Merge coordinates for edges ---
edges <- merge(edges, nodes, by.x = "from", by.y = "name")
edges <- merge(edges, nodes, by.x = "to", by.y = "name",
               suffixes = c("_from", "_to"))

# --- Shorten arrows so they stop at circle edges ---
node_radius <- 0.32
start_radius <- 0.2   # smaller trim at tail
end_radius   <- 0.25   # larger trim at arrowhead

edges <- edges %>%
  rowwise() %>%
  mutate(
    dx = x_to - x_from,
    dy = y_to - y_from,
    dist = sqrt(dx^2 + dy^2),
    x_from_adj = x_from + dx * (start_radius / dist),
    y_from_adj = y_from + dy * (start_radius / dist),
    x_to_adj   = x_to   - dx * (end_radius   / dist),
    y_to_adj   = y_to   - dy * (end_radius   / dist)
  ) %>%
  ungroup()

# --- Helper: draw semicircle polygons ---
geom_halfcircle <- function(x, y, r = 0.28, side = c("left", "right"),
                            fill = "#009E73", color = "black", ...) {
  side <- match.arg(side)
  theta <- if (side == "left") seq(pi/2, 3*pi/2, length.out = 100)
  else seq(-pi/2, pi/2, length.out = 100)
  df <- data.frame(x = x + r * cos(theta), y = y + r * sin(theta))
  geom_polygon(data = df, aes(x, y), fill = fill, color = color, ...)
}

# --- Curved mixed edge between X_left and Z: no arrowheads ---
xz_edge <- data.frame(
  x    = -1.2,
  y    =  0.7 - node_radius + 0.2,
  xend = -1.2,
  yend = -0.7 + node_radius - 0.24
)

# --- Plot ---
p_swig <- ggplot() +
  # directed arrows
  geom_segment(
    data = edges,
    aes(x = x_from_adj, y = y_from_adj, xend = x_to_adj, yend = y_to_adj),
    arrow = grid::arrow(length = unit(0.18, "inches"), type = "closed"),
    linewidth = 1.0, color = "black", lineend = "butt"
  ) +
  # curved X_left-Z connection with no heads
  geom_curve(
    data = xz_edge,
    aes(x = x, y = y, xend = xend, yend = yend),
    curvature = 0.4,
    linewidth = 1.0,
    color = "black",
    lineend = "round"
  ) +
  # split X node
  geom_halfcircle(x = -1,   y = 0.7, side = "left",  fill = "#E69F00", color = "black", linewidth = 1.0) +
  geom_halfcircle(x = -0.8, y = 0.7, side = "right", fill = "#E69F00", color = "black", linewidth = 1.0) +
  # regular nodes
  geom_point(
    data = dplyr::filter(nodes, !name %in% c("X_left","X_right")),
    aes(x = x, y = y),
    fill = "#009E73", shape = 21, size = 28, color = "black", stroke = 1.0
  ) +
  # labels
  geom_text(
    data = nodes %>%
      mutate(x_lab = case_when(
        name == "X_left"  ~ x - 0.12,
        name == "X_right" ~ x + 0.03,
        TRUE              ~ x
      )),
    aes(x = x_lab, y = y, label = label),
    color = "white", fontface = "bold", size = 7
  ) +
  coord_equal(xlim = c(-1.4, 1.4), ylim = c(-.7, .9), clip = "off") +
  theme_void(base_size = 20) +
  theme(plot.margin = margin(0, 15, 25, 15))


# ================================
# Combine graphs into one figure  
# ================================

multi_fig <- plot_grid(
  p_dag, p_swig,
  labels = c("A", "B"),
  label_size = 24,
  ncol = 2,
  align = "hv"
)

ggsave("graphs.png", plot = multi_fig,
       width = 12, height = 4, dpi = 600, bg = "white")
