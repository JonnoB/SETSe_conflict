#' Re wieghter
#'  
#' This function re-weights the values of the edges to explore different edge weigh ratios
#'  
#' @param g An igraph object
#' @param weight the edge weight attribute of the graph
#' @param min_k a positive numeric. The minimum value of K
#' @param mid_point an integer the midpoint between the minimum and maximum values
#' @param range_k a positive numeric. The maximum multilplier between the smallest and largest k values
#' @param reverse_order logical. high weights become low weight and visa versa
#'
#' @export

edge_reweighter <- function(g, weight, min_k, mid_point, range_k, reverse_order = FALSE){
  
  
  edge_weights <- get.edge.attribute(g, name = weight)
  
  full_range <- min_k*range_k - min_k
  
  #finds the fraction of the total range of edge weights each edge is. The lowest weights score 0
  #the highest weights score 1.
  fraction_weights <- (edge_weights - min(edge_weights))/(max(edge_weights)- min(edge_weights))
  
  #reverses the above score by subtracting one.
  #Thus 1 becomes 0 and 0 becomes 1
  if(reverse_order){
    fraction_weights <- 1- fraction_weights
  }
  
  new_k <- (fraction_weights-0.5)*full_range + mid_point

  g2 <- set.edge.attribute(g, name = weight, value = new_k)
  
  return(g2)
  
}