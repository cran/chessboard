## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse  = TRUE,
                      comment   = "#>",
                      out.width = "100%",
                      dpi       = 96,
                      fig.align = "center")

## ----'setup', echo=TRUE-------------------------------------------------------
# Setup ----
library("chessboard")
library("ggplot2")
library("patchwork")

## ----'ggplot-theme', echo=FALSE-----------------------------------------------
## Custom ggplot2 theme ----
custom_theme <- function() {
  theme_light() + 
  theme(plot.title   = element_text(face = "bold", family = "serif", size = 18),
        plot.caption = element_text(face = "italic", family = "serif"),
        axis.title   = element_blank(),
        axis.text    = element_text(family = "serif"))
}

## ----'cb-network', eval=TRUE, fig.height=8, fig.width=5, echo=FALSE, fig.cap="Figure 1. Network as a chessboard", out.width='60%'----
sites <- expand.grid("transect" = 1:3, "quadrat" = 1:5)

nodes <- create_node_labels(data     = sites,
                            transect = "transect", 
                            quadrat  = "quadrat")

gg_chessboard(nodes)

## ----echo = FALSE, out.width = "100%", fig.cap = "Figure 2. Workflow and main features of `chessboard`", fig.align = 'center'----
knitr::include_graphics("figures/diagramme.png")

## ----'map-adour-river', echo=FALSE, fig.height=9, fig.width=10, out.width='80%', fig.cap='Figure 3. Location of the French river L\'Adour'----
knitr::include_graphics("figures/map-adour-river.png")

## ----'import-adour-river', echo=FALSE-----------------------------------------
## Import the spatial layer of Adour river ----
path_to_file <- system.file("extdata", "adour_lambert93.gpkg", 
                            package = "chessboard")
adour_river  <- sf::st_read(path_to_file, quiet = TRUE)

## ----'import-adour-sites', echo=FALSE-----------------------------------------
## Import sites data ----
path_to_file <- system.file("extdata", "adour_survey_sampling.csv", 
                            package = "chessboard")
nodes  <- read.csv(path_to_file)

## Convert data.frame to sf object ----
nodes_sf <- sf::st_as_sf(nodes, coords = c("longitude", "latitude"),
                               crs = "epsg:2154")

## ----'map-adour-sites', fig.height=9, fig.width=12, out.width='80%', echo=FALSE, fig.cap='Figure 4. Survey sampling along the river L\'Adour'----
ggplot() +
  geom_sf(data = adour_river, col = "steelblue") +
  geom_sf(data = nodes_sf, shape = 19, size = 2) +
  labs(caption = "RGF93 / Lambert-93 Projection") +
  custom_theme() +
  geom_segment(aes(x = 454180, xend = 440170, y = 6216290, yend = 6263320), 
               arrow = arrow(length = unit(0.75, 'cm'), type = 'closed'),
               linewidth = 2.25) +
  geom_text(aes(x = 334500, y = 6285000), label = "River", hjust = 0,
            color = "steelblue", fontface = "bold", size = 6, 
            family = "serif") +
  geom_text(aes(x = 414950, y = 6312200), label = "Location 3", hjust = -0.20,
            color = "black", fontface = "bold", size = 6, family = "serif") +
  geom_text(aes(x = 474655, y = 6236708), label = "Location 1", 
            color = "black", fontface = "bold", size = 6, family = "serif") +
  geom_text(aes(x = 467250, y = 6287620), label = "Location 2", 
            color = "black", fontface = "bold", size = 6, family = "serif")

## ----'import-data', echo=TRUE-------------------------------------------------
# Import data ----
path_to_file <- system.file("extdata", "adour_survey_sampling.csv", 
                            package = "chessboard")

sampling  <- read.csv(path_to_file)

dim(sampling)

## ----'head-of-data', echo=TRUE------------------------------------------------
# First rows ----
head(sampling, 10)

## ----'tail-of-data', echo=TRUE------------------------------------------------
# Last rows ----
tail(sampling, 10)

## ----'select-data'------------------------------------------------------------
# Select the first location ----
sampling <- sampling[sampling$"location" == 1, ]

dim(sampling)

## ----'create-nodes-labels'----------------------------------------------------
# Create node labels ----
nodes <- create_node_labels(data     = sampling,
                            location = "location",
                            transect = "transect",
                            quadrat  = "quadrat")

nodes

## ----'plot-sampling-units', fig.height=8, fig.width=5, echo=TRUE, out.width='60%', fig.cap='Figure 5. Sampling survey as a chessboard'----
# Visualize chessboard ----
gg_chessboard(nodes)

## ----'get-node-labels'--------------------------------------------------------
# Extract node labels ----
get_node_list(nodes)

## ----'method-pawn'------------------------------------------------------------
# Explore pawn method to find neighbors ----
neighbors_pawn <- pawn(nodes    = nodes, 
                       focus    = "2-3", 
                       degree   = 1, 
                       directed = FALSE, 
                       reverse  = FALSE)
neighbors_pawn

## ----'nb-pawn', fig.height=8, fig.width=5, echo=TRUE, fig.cap="Figure 6. Detected neighbors (pawn method)", out.width='50%'----
gg_chessboard(nodes) +
  geom_node(nodes, focus = "2-3") +
  geom_neighbors(nodes, neighbors_pawn)

## ----'cb-pawn', fig.height=4.3, fig.width=12, echo=FALSE, fig.cap="Figure 7. Pawn movements", out.width='100%'----
demo_sites <- expand.grid("transect" = 1:9, "quadrat" = 1:9)

demo_nodes <- create_node_labels(data     = demo_sites,
                                 transect = "transect", 
                                 quadrat  = "quadrat")

demo_focus  <- "5-5"

pawn_1 <- 
  gg_chessboard(demo_nodes, "A. Undirected network", "") + 
  geom_node(demo_nodes, demo_focus) +
  geom_neighbors(demo_nodes, pawn(demo_nodes, demo_focus, degree = 4, 
                                  directed = FALSE, reverse = FALSE))

pawn_2 <- 
  gg_chessboard(demo_nodes, "B. Directed network", "") + 
  geom_node(demo_nodes, demo_focus) +
  geom_neighbors(demo_nodes, pawn(demo_nodes, demo_focus, degree = 4, 
                                  directed = TRUE, reverse = FALSE))

pawn_3 <- 
  gg_chessboard(demo_nodes, "C. Directed network (reverse)", "") + 
  geom_node(demo_nodes, demo_focus) +
  geom_neighbors(demo_nodes, pawn(demo_nodes, demo_focus, degree = 4, 
                                  directed = TRUE, reverse = TRUE))

(pawn_1 | pawn_2 | pawn_3)

## ----'cb-bishop', eval=TRUE, fig.height=4.3, fig.width=12, echo=FALSE, fig.cap="Figure 8. Bishop movements", out.width='100%'----
demo_sites <- expand.grid("transect" = 1:9, "quadrat" = 1:9)

demo_nodes <- create_node_labels(data     = demo_sites,
                                 transect = "transect", 
                                 quadrat  = "quadrat")

demo_focus  <- "5-5"

bishop_1 <- 
  gg_chessboard(demo_nodes, "A. Undirected network", "") + 
  geom_node(demo_nodes, demo_focus) +
  geom_neighbors(demo_nodes, bishop(demo_nodes, demo_focus, degree = 4, 
                                    directed = FALSE, reverse = FALSE))

bishop_2 <- 
  gg_chessboard(demo_nodes, "B. Directed network", "") + 
  geom_node(demo_nodes, demo_focus) +
  geom_neighbors(demo_nodes, bishop(demo_nodes, demo_focus, degree = 4, 
                                    directed = TRUE, reverse = FALSE))

bishop_3 <- 
  gg_chessboard(demo_nodes, "C. Directed network (reverse)", "") + 
  geom_node(demo_nodes, demo_focus) +
  geom_neighbors(demo_nodes, bishop(demo_nodes, demo_focus, degree = 4, 
                                    directed = TRUE, reverse = TRUE))

(bishop_1 | bishop_2 | bishop_3)

## ----'create-edges-list-pawn'-------------------------------------------------
# Create edge list ----
edges_pawn <- create_edge_list(nodes    = nodes, 
                               method   = "pawn", 
                               degree   = 1, 
                               directed = TRUE,
                               reverse  = FALSE,
                               self     = FALSE)

edges_pawn

## ----'df-to-sf'---------------------------------------------------------------
# Convert nodes to sf object ----
nodes_sf <- sf::st_as_sf(nodes, coords = c("longitude", "latitude"),
                               crs = "epsg:2154")

head(nodes_sf)

## ----'edges-list-to-sf'-------------------------------------------------------
# Convert edge list to sf ----
edges_pawn_sf <- edges_to_sf(edges = edges_pawn, 
                             sites = nodes_sf)

edges_pawn_sf

## ----'map-edges-list-pawn', fig.height=8, fig.width=6.5, echo=TRUE, fig.cap="Figure 9. Edge list (pawn method)", out.width='80%'----
# Map of nodes and edges ----
ggplot(nodes_sf) +
  geom_sf(size = 12) +
  geom_sf(data = edges_pawn_sf) +
  theme_light()

## ----'create-edges-list-bishop'-----------------------------------------------
# Create edge list (Bishop method) ----
edges_bishop <- create_edge_list(nodes    = nodes, 
                                 method   = "bishop", 
                                 degree   = 1, 
                                 directed = TRUE,
                                 reverse  = FALSE,
                                 self     = FALSE)

edges_bishop

# Merge Pawn and Bishop edges ----
edges <- append_edge_lists(edges_pawn, edges_bishop)

# Convert edges to spatial layer ----
edges_sf <- edges_to_sf(edges, nodes_sf)

## ----'map-edges-list-pawn-bishop', fig.height=8, fig.width=6.5, echo=TRUE, fig.cap="Figure 10. Edges list (combined methods)", out.width='80%'----
# Map of nodes and edges ----
ggplot(nodes_sf) +
  geom_sf(size = 12) +
  geom_sf(data = edges_sf) +
  theme_light()

## ----'connectivity-matrix'----------------------------------------------------
# Create connectivity matrix ----
conn_matrix <- connectivity_matrix(edges)

conn_matrix

## ----'plot-connectivity-matrix', fig.height=8, fig.width=8, fig.cap="Figure 11. Connectivity matrix", out.width='80%'----
# Visualize connectivity matrix ----
gg_matrix(conn_matrix)

## ----'connectivity-matrix-to-df'----------------------------------------------
# Convert connectivity matrix to edge list ----
matrix_to_edge_list(conn_matrix)

## ----'transform-crs'----------------------------------------------------------
# Convert edges to spatial layer ----
edges_sf <- edges_to_sf(edges, nodes_sf)

# Project the CRS ----
edges_sf_lonlat <- sf::st_transform(edges_sf, crs = "epsg:4326")

# Check ----
edges_sf
edges_sf_lonlat

## ----'export-sf', eval=FALSE--------------------------------------------------
#  # Export layer as a GeoPackage ----
#  sf::st_write(edges_sf, "edge_list.gpkg")

## ----'change-theme', fig.height=8, fig.width=8, fig.cap="Figure 12. Custom connectivity matrix", out.width='80%'----
# Change default ggplot2 theme ----
gg_matrix(conn_matrix) +
  theme_bw() +
  theme(legend.position = "none")

## ----'to-igraph'--------------------------------------------------------------
# Convert edge list to igraph object ----
igraph_obj <- igraph::graph_from_data_frame(d        = edges, 
                                            directed = TRUE, 
                                            vertices = nodes)

# Check -----
class(igraph_obj)

print(igraph_obj)

## ----'plot-igraph', fig.height=8, fig.width=8, fig.cap="Figure 13. Network visualization w/ `igraph`", out.width='80%'----
# Plot the network w/ igraph ----
plot(igraph_obj)

