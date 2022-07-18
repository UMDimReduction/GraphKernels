
library(igraph)

#-------------------------------------------------------------------------------

#===================================================================
#' stores igraph data set information from data directory in the 
#' global environment. Requires a folder of .graphml files, label file 
#' indicating the class of each .graphml object, and a .list file containing
#' names of the .graphml objects. All files must have the same name.
#' 
#' @param name name of data set 
#===================================================================
read.dataset <- function(name){
  
  lb        <- unlist(read.table(paste0("data/", name, ".label")))
  names(lb) <- NULL
  n         <- length(lb)
  G         <- vector("list", n)
  
  for (i in 1:n) {
    G[[i]] <- read_graph(paste0("data/", name, "/", name, "_", i, ".graphml"), format = "graphml")
    graph_attr(G[[i]], "label") <- lb[i]
  }
  
  for(i in 1:n){
    G[[i]] <- delete_vertex_attr(G[[i]], "id")
  }
  
  G
}
