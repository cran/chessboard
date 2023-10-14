## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse  = TRUE,
                      comment   = "#>",
                      out.width = "50%",
                      dpi       = 72,
                      fig.align = "center")

## ----'setup', echo=FALSE------------------------------------------------------
library("chessboard")
library("ggplot2")
library("patchwork")

## ----'fictitious-data', echo=FALSE--------------------------------------------
sites <- expand.grid("transect" = 1:9, "quadrat" = 1:9)

nodes <- create_node_labels(data     = sites,
                            transect = "transect", 
                            quadrat  = "quadrat")

focus  <- "5-5"

transects_quadrats <- expand.grid("transect" = 1:3, 
                                  "quadrat"  = 1:5)

transects_only     <- data.frame("transect"  = 1:3)
quadrats_only      <- data.frame("quadrat"   = 1:5)

nodes_transects_quadrats <- create_node_labels(data     = transects_quadrats,
                                               transect = "transect", 
                                               quadrat  = "quadrat")

nodes_transects_only <- create_node_labels(data     = transects_only,
                                           transect = "transect")

nodes_quadrats_only  <- create_node_labels(data     = quadrats_only,
                                           quadrat  = "quadrat")

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  pawn(nodes, focus = "5-5", degree = 4, ...)

## ----'cb-pawn', eval=TRUE, fig.height=4.3, fig.width=12, echo=FALSE, fig.cap="Figure 1. Moves of the **Pawn** (with degree = 4)", out.width='100%'----
pawn_1 <- 
  gg_chessboard(nodes, "A. Undirected network", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, pawn(nodes, focus, degree = 4, directed = FALSE, 
                             reverse = FALSE))

pawn_2 <- 
  gg_chessboard(nodes, "B. Directed network", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, pawn(nodes, focus, degree = 4, directed = TRUE, 
                             reverse = FALSE))

pawn_3 <- 
  gg_chessboard(nodes, "C. Directed network (reverse)", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, pawn(nodes, focus, degree = 4, directed = TRUE, 
                             reverse = TRUE))

(pawn_1 | pawn_2 | pawn_3)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  create_edge_list(nodes, method = "pawn", degree = 4, ...)

## ----'cb-pawn-mat', eval=TRUE, fig.height=4.3, fig.width=12, echo=FALSE, fig.cap="Figure 2. Connectivity matrices of the **Pawn** (with degree = 4)", out.width='100%'----
nb_1 <- create_edge_list(nodes, 
                          method   = "pawn", 
                          degree   = 4, 
                          directed = FALSE, 
                          reverse  = FALSE)

pawn_1 <- gg_matrix(connectivity_matrix(nb_1), "A. Undirected network") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

nb_2 <- create_edge_list(nodes, 
                          method   = "pawn", 
                          degree   = 4, 
                          directed = TRUE, 
                          reverse  = FALSE)

pawn_2 <- gg_matrix(connectivity_matrix(nb_2), "B. Directed network") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

nb_3 <- create_edge_list(nodes, 
                          method   = "pawn", 
                          degree   = 4, 
                          directed = TRUE, 
                          reverse  = TRUE)

pawn_3 <- gg_matrix(connectivity_matrix(nb_3), "C. Directed network (reverse)") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

(pawn_1 | pawn_2 | pawn_3)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  fool(nodes, focus = "5-5", degree = 4, ...)

## ----'cb-fool', eval=TRUE, fig.height=4.3, fig.width=12, echo=FALSE, fig.cap="Figure 3. Moves of the **Fool** (with degree = 4)", out.width='100%'----
fool_1 <- 
  gg_chessboard(nodes, "A. Undirected network", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, fool(nodes, focus, degree = 4, directed = FALSE, 
                             reverse = FALSE))

fool_2 <- 
  gg_chessboard(nodes, "B. Directed network", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, fool(nodes, focus, degree = 4, directed = TRUE, 
                             reverse = FALSE))

fool_3 <- 
  gg_chessboard(nodes, "C. Directed network (reverse)", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, fool(nodes, focus, degree = 4, directed = TRUE, 
                             reverse = TRUE))

(fool_1 | fool_2 | fool_3)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  create_edge_list(nodes, method = "fool", degree = 4, ...)

## ----'cb-fool-mat', eval=TRUE, fig.height=4.3, fig.width=12, echo=FALSE, fig.cap="Figure 4. Connectivity matrices of the **Fool** (with degree = 4)", out.width='100%'----
nb_1 <- create_edge_list(nodes, 
                          method   = "fool", 
                          degree   = 4, 
                          directed = FALSE, 
                          reverse  = FALSE)

fool_1 <- gg_matrix(connectivity_matrix(nb_1), "A. Undirected network") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

nb_2 <- create_edge_list(nodes, 
                          method   = "fool", 
                          degree   = 4, 
                          directed = TRUE, 
                          reverse  = FALSE)

fool_2 <- gg_matrix(connectivity_matrix(nb_2), "B. Directed network") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

nb_3 <- create_edge_list(nodes, 
                          method   = "fool", 
                          degree   = 4, 
                          directed = TRUE, 
                          reverse  = TRUE)

fool_3 <- gg_matrix(connectivity_matrix(nb_3), "C. Directed network (reverse)") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

(fool_1 | fool_2 | fool_3)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  rook(nodes, focus = "5-5", degree = 4, ...)

## ----'cb-rook', eval=TRUE, fig.height=4.3, fig.width=12, echo=FALSE, fig.cap="Figure 5. Moves of the **Rook** (with degree = 4)", out.width='100%'----
rook_1 <- 
  gg_chessboard(nodes, "A. Undirected network", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, rook(nodes, focus, degree = 4, directed = FALSE, 
                             reverse = FALSE))

rook_2 <- 
  gg_chessboard(nodes, "B. Directed network", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, rook(nodes, focus, degree = 4, directed = TRUE, 
                             reverse = FALSE))

rook_3 <- 
  gg_chessboard(nodes, "C. Directed network (reverse)", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, rook(nodes, focus, degree = 4, directed = TRUE, 
                             reverse = TRUE))

(rook_1 | rook_2 | rook_3)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  create_edge_list(nodes, method = "rook", degree = 4, ...)

## ----'cb-rook-mat', eval=TRUE, fig.height=4.3, fig.width=12, echo=FALSE, fig.cap="Figure 6. Connectivity matrices of the **Rook** (with degree = 4)", out.width='100%'----
nb_1 <- create_edge_list(nodes, 
                          method   = "rook", 
                          degree   = 4, 
                          directed = FALSE, 
                          reverse  = FALSE)

rook_1 <- gg_matrix(connectivity_matrix(nb_1), "A. Undirected network") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

nb_2 <- create_edge_list(nodes, 
                          method   = "rook", 
                          degree   = 4, 
                          directed = TRUE, 
                          reverse  = FALSE)

rook_2 <- gg_matrix(connectivity_matrix(nb_2), "B. Directed network") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

nb_3 <- create_edge_list(nodes, 
                          method   = "rook", 
                          degree   = 4, 
                          directed = TRUE, 
                          reverse  = TRUE)

rook_3 <- gg_matrix(connectivity_matrix(nb_3), "C. Directed network (reverse)") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

(rook_1 | rook_2 | rook_3)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  bishop(nodes, focus = "5-5", degree = 4, ...)

## ----'cb-bishop', eval=TRUE, fig.height=4.3, fig.width=12, echo=FALSE, fig.cap="Figure 7. Moves of the **Bishop** (with degree = 4)", out.width='100%'----
bishop_1 <- 
  gg_chessboard(nodes, "A. Undirected network", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, bishop(nodes, focus, degree = 4, directed = FALSE, 
                               reverse = FALSE))

bishop_2 <- 
  gg_chessboard(nodes, "B. Directed network", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, bishop(nodes, focus, degree = 4, directed = TRUE, 
                               reverse = FALSE))

bishop_3 <- 
  gg_chessboard(nodes, "C. Directed network (reverse)", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, bishop(nodes, focus, degree = 4, directed = TRUE, 
                               reverse = TRUE))

(bishop_1 | bishop_2 | bishop_3)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  create_edge_list(nodes, method = "bishop", degree = 4, ...)

## ----'cb-bishop-mat', eval=TRUE, fig.height=4.3, fig.width=12, echo=FALSE, fig.cap="Figure 8. Connectivity matrices of the **Bishop** (with degree = 4)", out.width='100%'----
nb_1 <- create_edge_list(nodes, 
                          method   = "bishop", 
                          degree   = 4, 
                          directed = FALSE, 
                          reverse  = FALSE)

bishop_1 <- gg_matrix(connectivity_matrix(nb_1), "A. Undirected network") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

nb_2 <- create_edge_list(nodes, 
                          method   = "bishop", 
                          degree   = 4, 
                          directed = TRUE, 
                          reverse  = FALSE)

bishop_2 <- gg_matrix(connectivity_matrix(nb_2), "B. Directed network") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

nb_3 <- create_edge_list(nodes, 
                          method   = "bishop", 
                          degree   = 4, 
                          directed = TRUE, 
                          reverse  = TRUE)

bishop_3 <- gg_matrix(connectivity_matrix(nb_3), "C. Directed network (reverse)") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

(bishop_1 | bishop_2 | bishop_3)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  knight(nodes, focus = "5-5", degree = 4, ...)

## ----'cb-knight', eval=TRUE, fig.height=4.3, fig.width=12, echo=FALSE, fig.cap="Figure 9. Moves of the **Knight** (with degree = 4)", out.width='100%'----
knight_1 <- 
  gg_chessboard(nodes, "A. Undirected network", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, knight(nodes, focus, degree = 4, directed = FALSE, 
                               reverse = FALSE))

knight_2 <- 
  gg_chessboard(nodes, "B. Directed network", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, knight(nodes, focus, degree = 4, directed = TRUE, 
                               reverse = FALSE))

knight_3 <- 
  gg_chessboard(nodes, "C. Directed network (reverse)", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, knight(nodes, focus, degree = 4, directed = TRUE, 
                               reverse = TRUE))

(knight_1 | knight_2 | knight_3)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  create_edge_list(nodes, method = "knight", degree = 4, ...)

## ----'cb-knight-mat', eval=TRUE, fig.height=4.3, fig.width=12, echo=FALSE, fig.cap="Figure 10. Connectivity matrices of the **Knight** (with degree = 4)", out.width='100%'----
nb_1 <- create_edge_list(nodes, 
                          method   = "knight", 
                          degree   = 4, 
                          directed = FALSE, 
                          reverse  = FALSE)

knight_1 <- gg_matrix(connectivity_matrix(nb_1), "A. Undirected network") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

nb_2 <- create_edge_list(nodes, 
                          method   = "knight", 
                          degree   = 4, 
                          directed = TRUE, 
                          reverse  = FALSE)

knight_2 <- gg_matrix(connectivity_matrix(nb_2), "B. Directed network") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

nb_3 <- create_edge_list(nodes, 
                          method   = "knight", 
                          degree   = 4, 
                          directed = TRUE, 
                          reverse  = TRUE)

knight_3 <- gg_matrix(connectivity_matrix(nb_3), "C. Directed network (reverse)") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

(knight_1 | knight_2 | knight_3)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  queen(nodes, focus = "5-5", degree = 4, ...)

## ----'cb-queen', eval=TRUE, fig.height=4.3, fig.width=12, echo=FALSE, fig.cap="Figure 11. Moves of the **Queen** (with degree = 4)", out.width='100%'----
queen_1 <- 
  gg_chessboard(nodes, "A. Undirected network", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, queen(nodes, focus, degree = 4, directed = FALSE, 
                              reverse = FALSE))

queen_2 <- 
  gg_chessboard(nodes, "B. Directed network", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, queen(nodes, focus, degree = 4, directed = TRUE, 
                              reverse = FALSE))

queen_3 <- 
  gg_chessboard(nodes, "C. Directed network (reverse)", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, queen(nodes, focus, degree = 4, directed = TRUE, 
                              reverse = TRUE))

(queen_1 | queen_2 | queen_3)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  create_edge_list(nodes, method = "queen", degree = 4, ...)

## ----'cb-queen-mat', eval=TRUE, fig.height=4.3, fig.width=12, echo=FALSE, fig.cap="Figure 12. Connectivity matrices of the **Queen** (with degree = 4)", out.width='100%'----
nb_1 <- create_edge_list(nodes, 
                          method   = "queen", 
                          degree   = 4, 
                          directed = FALSE, 
                          reverse  = FALSE)

queen_1 <- gg_matrix(connectivity_matrix(nb_1), "A. Undirected network") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

nb_2 <- create_edge_list(nodes, 
                          method   = "queen", 
                          degree   = 4, 
                          directed = TRUE, 
                          reverse  = FALSE)

queen_2 <- gg_matrix(connectivity_matrix(nb_2), "B. Directed network") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

nb_3 <- create_edge_list(nodes, 
                          method   = "queen", 
                          degree   = 4, 
                          directed = TRUE, 
                          reverse  = TRUE)

queen_3 <- gg_matrix(connectivity_matrix(nb_3), "C. Directed network (reverse)") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

(queen_1 | queen_2 | queen_3)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  wizard(nodes, focus = "5-5", degree = 4, ...)

## ----'cb-wizard', eval=TRUE, fig.height=4.3, fig.width=12, echo=FALSE, fig.cap="Figure 13. Moves of the **Wizard** (with degree = 4)", out.width='100%'----
wizard_1 <- 
  gg_chessboard(nodes, "A. Undirected network", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, wizard(nodes, focus, degree = 4, directed = FALSE, 
                               reverse = FALSE))

wizard_2 <- 
  gg_chessboard(nodes, "B. Directed network", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, wizard(nodes, focus, degree = 4, directed = TRUE, 
                               reverse = FALSE))

wizard_3 <- 
  gg_chessboard(nodes, "C. Directed network (reverse)", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, wizard(nodes, focus, degree = 4, directed = TRUE, 
                               reverse = TRUE))

(wizard_1 | wizard_2 | wizard_3)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  create_edge_list(nodes, method = "wizard", degree = 4, ...)

## ----'cb-wizard-mat', eval=TRUE, fig.height=4.3, fig.width=12, echo=FALSE, fig.cap="Figure 14. Connectivity matrices of the **Wizard** (with degree = 4)", out.width='100%'----
nb_1 <- create_edge_list(nodes, 
                          method   = "wizard", 
                          degree   = 4, 
                          directed = FALSE, 
                          reverse  = FALSE)

wizard_1 <- gg_matrix(connectivity_matrix(nb_1), "A. Undirected network") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

nb_2 <- create_edge_list(nodes, 
                          method   = "wizard", 
                          degree   = 4, 
                          directed = TRUE, 
                          reverse  = FALSE)

wizard_2 <- gg_matrix(connectivity_matrix(nb_2), "B. Directed network") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

nb_3 <- create_edge_list(nodes, 
                          method   = "wizard", 
                          degree   = 4, 
                          directed = TRUE, 
                          reverse  = TRUE)

wizard_3 <- gg_matrix(connectivity_matrix(nb_3), "C. Directed network (reverse)") +
  theme(axis.text = element_blank(), axis.text.x = element_blank(),
        plot.caption = ggplot2::element_text(family = "mono", size = 12, 
            face = "bold", hjust = 0.5))

(wizard_1 | wizard_2 | wizard_3)

## ----'cb-custom', eval=TRUE, fig.height=4.3, fig.width=12, echo=FALSE, fig.cap="Figure 15. Custom moves", out.width='100%'----
wizard_1 <- 
  gg_chessboard(nodes, "Pawn & Bishop", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, pawn(nodes, focus, degree = 3, directed = TRUE, 
                             reverse = FALSE)) +
  geom_neighbors(nodes, bishop(nodes, focus, degree = 4, directed = TRUE, 
                               reverse = FALSE))

wizard_2 <- 
  gg_chessboard(nodes, "Knight L & Bishop L", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, knight_left(nodes, focus, degree = 4, directed = TRUE, 
                                    reverse = FALSE)) +
  geom_neighbors(nodes, bishop_left(nodes, focus, degree = 4, directed = TRUE, 
                                    reverse = FALSE))

wizard_3 <- 
  gg_chessboard(nodes, "Knight L, Bishop L, Pawn & Fool", "") + 
  geom_node(nodes, focus) +
  geom_neighbors(nodes, knight_left(nodes, focus, degree = 4, directed = TRUE, 
                                    reverse = FALSE)) +
  geom_neighbors(nodes, bishop_left(nodes, focus, degree = 4, directed = TRUE, 
                                    reverse = FALSE)) +
  geom_neighbors(nodes, pawn(nodes, focus, degree = 4, directed = TRUE, 
                             reverse = FALSE)) +
  geom_neighbors(nodes, fool(nodes, focus, degree = 4, directed = TRUE, 
                             reverse = TRUE))

(wizard_1 | wizard_2 | wizard_3)

