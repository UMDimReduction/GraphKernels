
library(igraph)

#-------------------------------------------------------------------------------

read.dataset <- function(name){
  lb <- unlist(read.table(paste0("data/", name, ".label")))
  names(lb) <- NULL
  n <- length(lb)
  G <- vector("list", n)
  for (i in 1:n) {
    G[[i]] <- read_graph(paste0("data/", name, "/", name, "_", i, ".graphml"), format = "graphml")
    graph_attr(G[[i]], "label") <- lb[i]
  }
  for(i in 1:n){
    G[[i]] <- delete_vertex_attr(G[[i]], "id")
  }
  G
}
