## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse  = TRUE,
                      comment   = "#>",
                      out.width = "50%",
                      dpi       = 96,
                      fig.align = "center")

## ----'setup', echo=TRUE-------------------------------------------------------
# Setup ----
library("chessboard")
library("ggplot2")

## ----'import-adour-sites', echo=TRUE------------------------------------------
# Location of the data ----
path_to_file <- system.file("extdata", "adour_survey_sampling.csv", 
                            package = "chessboard")

# Read the data ----
sampling  <- read.csv(path_to_file)

head(sampling)

## ----'subset-adour-sites', echo=TRUE------------------------------------------
# Subset location 1 ----
sampling <- sampling[sampling$"location" == 1, ]

sampling

## ----'create-nodes-labels', echo=TRUE-----------------------------------------
# Create node labels ----
nodes <- create_node_labels(data     = sampling,
                            location = "location",
                            transect = "transect", 
                            quadrat  = "quadrat")

nodes

## ----'gg-chessboard', echo=TRUE, dpi=150, fig.height=8, fig.width=5-----------
# Plot the sampling as a chessboard ----
gg_chessboard(nodes)

## ----'geom-piece', echo=TRUE, dpi=150, fig.height=8, fig.width=5--------------
# Locate one node ----
gg_chessboard(nodes) +
  geom_node(nodes, focus = "2-3")

## ----'geom-pieces', echo=TRUE, dpi=150, fig.height=8, fig.width=5-------------
# Locate various nodes ----
gg_chessboard(nodes) +
  geom_node(nodes, focus = "2-3") +
  geom_node(nodes, focus = "1-5") +
  geom_node(nodes, focus = "3-1")

## ----'nb-rook', echo=TRUE-----------------------------------------------------
# Neighbors detection ----
nb_rook <- rook(nodes, focus = "2-3")

nb_rook

## ----'geom-neighbors', echo=TRUE, dpi=150, fig.height=8, fig.width=5----------
# Locate neighbors ----
gg_chessboard(nodes) +
  geom_node(nodes, focus = "2-3") +
  geom_neighbors(nodes, neighbors = nb_rook)

## ----'geom-edges', echo=TRUE, dpi=150, fig.height=8, fig.width=5--------------
# Locate neighbors ----
gg_chessboard(nodes) +
  geom_edges(nodes, focus = "2-3", neighbors = nb_rook) +
  geom_node(nodes, focus = "2-3")

## ----'nb-bishop', echo=TRUE---------------------------------------------------
# Neighbors detection ----
nb_bishop <- bishop(nodes, focus = "2-3")

nb_bishop

## ----'geom-edges-twice', echo=TRUE, dpi=150, fig.height=8, fig.width=5--------
# Locate neighbors ----
gg_chessboard(nodes) +
  geom_edges(nodes, focus = "2-3", neighbors = nb_rook) +
  geom_edges(nodes, focus = "2-3", neighbors = nb_bishop) +
  geom_neighbors(nodes, neighbors = nb_rook) +
  geom_neighbors(nodes, neighbors = nb_bishop) +
  geom_node(nodes, focus = "2-3")

## ----'edges-queen', echo=TRUE-------------------------------------------------
# Edges list ----
edges <- create_edge_list(nodes, method = "queen")

head(edges)

## ----'connectivity-matrix', echo=TRUE-----------------------------------------
# Connectivity matrix ----
mat <- connectivity_matrix(edges)

mat

## ----'gg-matrix', echo=TRUE, dpi=150, fig.height=6, fig.width=6---------------
# Visualize matrix ----
gg_matrix(mat)

## ----'convert-to-sf', echo=TRUE-----------------------------------------------
# Convert sampling to sf object ----
nodes_sf <- sf::st_as_sf(nodes, 
                         coords = c("longitude", "latitude"),
                         crs = "epsg:2154")

nodes_sf

## ----'edges-to-sf', echo=TRUE-------------------------------------------------
# Convert edges to sf object ----
edges_sf <- edges_to_sf(edges, nodes_sf)

edges_sf

## ----'mapping-edges', echo=TRUE, dpi = 150, fig.height=8, fig.width=6.5-------
# Map of nodes and edges ----
ggplot() +
  geom_sf(data = edges_sf) +
  geom_sf(data = nodes_sf, size = 12) +
  theme_light()

## ----'mapping-edges-labels', echo=TRUE, dpi = 150, fig.height=8, fig.width=6.5----
# Map of nodes and edges ----
ggplot() +
  geom_sf(data = edges_sf) +
  geom_sf(data = nodes_sf, size = 12) +
  geom_sf_text(data = nodes_sf, aes(label = node), 
               color = "white", fontface = "bold",
               family = "mono") +
  theme_light()

