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
  x = c(-1, 0, -1, 1),
  y = c(.7, 0, -.7, 0)
)

# --- Edges (from → to) ---
edges <- data.frame(
  from = c("X","X","Z","Z","Y"),
  to   = c("Y","Z","X","Y","S")
)

# --- Merge coordinates for edges ---
edges <- merge(edges, nodes, by.x = "from", by.y = "name")
edges <- merge(edges, nodes, by.x = "to", by.y = "name",
               suffixes = c("_from", "_to"))

# --- Shorten arrows so they stop at circle edge ---
node_radius <- 0.2   # roughly matches visual radius of size=10 point
edges <- edges %>%
  rowwise() %>%
  mutate(
    dx = x_to - x_from,
    dy = y_to - y_from,
    dist = sqrt(dx^2 + dy^2),
    shrink = node_radius / dist,
    # move start and end points slightly inward/outward
    x_from_adj = x_from + dx * shrink,
    y_from_adj = y_from + dy * shrink,
    x_to_adj   = x_to   - dx * shrink,
    y_to_adj   = y_to   - dy * shrink
  )

# --- Plot ---
p_dag <- ggplot() +
  # arrows (now trimmed to stop at circle edges)
  geom_segment(
    data = edges,
    aes(x = x_from_adj, y = y_from_adj,
        xend = x_to_adj, yend = y_to_adj),
    arrow = arrow(length = unit(4, "mm"), type = "closed", ends = "last"),
    linewidth = 0.6, color = "black", lineend = "round"
  ) +
  # node circles
  geom_point(
    data = nodes,
    aes(x = x, y = y),
    fill = "#009E73", shape = 21, size = 25,
    color = "black", stroke = 0.3
  ) +
  # node labels
  geom_text(
    data = nodes,
    aes(x = x, y = y, label = name),
    color = "white", fontface = "bold", size = 7
  ) +
  coord_equal(xlim = c(-2, 2), ylim = c(-2, 1)) +
  theme_void(base_size = 14) +
  theme(plot.margin = margin(20, 15, 15, 15)) +
  coord_equal(clip = "off")


# ================================
# Single world intervention graph
# ================================

# --- Node coordinates ---
nodes <- data.frame(
  name  = c("X_left", "X_right", "Y", "Z", "S"),
  label = c("X", "x", "Y(x)", "Z", "S(x)"),
  x     = c(-1, -0.7, 0, -1, 1),
  y     = c(0.7, 0.7, 0, -0.7, 0)
)

# --- Edges (from → to) ---
edges <- data.frame(
  from = c("Z","X_left","X_right","Y","Z"),
  to   = c("X_left","Z","Y","S","Y")
)

# --- Merge coordinates for edges ---
edges <- merge(edges, nodes, by.x = "from", by.y = "name")
edges <- merge(edges, nodes, by.x = "to", by.y = "name",
               suffixes = c("_from", "_to"))

# --- Shorten arrows so they stop at circle edges ---
node_radius <- 0.27
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

# --- Helper: draw semicircle polygons ---
geom_halfcircle <- function(x, y, r = 0.28, side = c("left", "right"),
                            fill = "#009E73", color = "black", ...) {
  side <- match.arg(side)
  theta <- if (side == "left") seq(pi/2, 3*pi/2, length.out = 100)
  else seq(-pi/2, pi/2, length.out = 100)
  df <- data.frame(x = x + r * cos(theta), y = y + r * sin(theta))
  geom_polygon(data = df, aes(x, y), fill = fill, color = color, ...)
}

# --- Plot ---
p_swig <- ggplot() +
  # arrows
  geom_segment(
    data = edges,
    aes(x = x_from_adj, y = y_from_adj, xend = x_to_adj, yend = y_to_adj),
    arrow = arrow(length = unit(4, "mm"), type = "closed", ends = "last"),
    linewidth = 0.6, color = "black", lineend = "round"
  ) +
  # split X node
  geom_halfcircle(x = -1,   y = 0.7, side = "left",  fill = "#E69F00") +
  geom_halfcircle(x = -0.7, y = 0.7, side = "right", fill = "#E69F00") +
  # regular nodes
  geom_point(
    data = filter(nodes, !name %in% c("X_left","X_right")),
    aes(x = x, y = y),
    fill = "#009E73", shape = 21, size = 30, color = "black", stroke = 0.3
  ) +
  # labels (shifted so X and x are centered in halves)
  geom_text(data = nodes %>% 
              mutate(x = case_when(
                name == "X_left"  ~ x - 0.12,
                name == "X_right" ~ x + 0.12,
                TRUE              ~ x
              )),
            aes(x = x, y = y, label = label),
            color = "white", fontface = "bold", size = 7) +
  coord_equal(clip="off") +
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
