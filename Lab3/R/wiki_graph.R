#' Wiki_graph
#'
#' A dataset containing the wiki_graph.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{v1}{nodes}
#'   \item{v2}{nodes}
#'   \item{w}{weight of an edge}
#'   ...
#' }
#' @source \url{https://sv.wikipedia.org/wiki/Dijkstras_algoritm}
"wiki_graph"

wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))