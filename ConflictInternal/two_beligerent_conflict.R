#' two_beligerent_conflict
#'
#' creates the forces for a conflict involving only two beligerents. The first node is always positive
#'
#' @param g an I ggraph object
#' @param beligerents numeric vector of lwngth two. Node indices of the the beligerent nodes
#'
#'
#'@export
two_beligerent_conflict <- function(g, beligerents){
  
  force_vect <- rep(0, vcount(g))
  
  force_vect[beligerents] <- c(1,-1)
  
  g <- set.vertex.attribute(g, "force", value = force_vect)
  
  return(g)
  
}