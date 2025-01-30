# Load necessary libraries for creating a network graph ####
library(igraph)  

# Define industry names ####
industries <- c("Industry 1", "Industry 2")

# Create a graph from the adjacency matrix (A) ####
graph <- graph_from_adjacency_matrix(A, mode = "directed", weighted = TRUE)

# Set vertex (node) labels ####
V(graph)$name <- industries

# Set edge weights for visualization ####
E(graph)$width <- E(graph)$weight * 5  # Scale edge weights for better visibility

# Add edge labels (weights) and re-plot chart ####
edge_labels <- round(E(graph)$weight, 2)
edge_labels <- paste0(c("a11 = ",
                        "a12 = ",
                        "a21 = ",
                        "a22 = "
)
, edge_labels)

# Plot the network graph ####
layout(matrix(c(1), 1, 1, byrow = TRUE))
set.seed(123)  # For reproducible layout
plot(graph, 
     edge.arrow.size = 0.5,  # Size of arrowheads
     edge.color = "gray50",  # Edge color
     vertex.color = c("lightgreen","pink"),  # Node color
     vertex.size = 30,  # Node size
     vertex.label.color = "black",  # Node label color
     vertex.label.cex = 1.2,  # Node label size
     vertex.label.dist = 2,  # Distance of label from node
     main = "Input-output network with 2 industries",  # Graph title
     layout = layout_with_fr(graph),  # Fruchterman-Reingold layout
     edge.label = edge_labels,  # Add edge labels
     edge.label.color = "black",  # Edge label color
     edge.label.cex = 0.8,  # Edge label size
     )
