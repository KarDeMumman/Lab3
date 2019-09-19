#' Find the shortest distance
#' @param graph A dataframe.
#' @param init_node the initial node.
#' @return The shortest distance to each node starting from the initial node.
#' @examples
#' dijkstra(wiki_graph,1)
#' dijkstra(wiki_graph,3)
#' \href{https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm}

dijkstra<- function (graph, init_node){
  v1<-graph[,1] #The start of the edge
  v2<-graph[,2] #The end of the edge
  w <-graph[,3] #The weight of the edge
  nodes<-unique(v1) #The nodes in the graph
  stopifnot(init_node %in% nodes)
  stopifnot(init_node %in% nodes)
  stopifnot(is.numeric(init_node))
  stopifnot(is.data.frame(graph))
  stopifnot(length(graph) == 3)
  
  
  distance<-rep(Inf,length(nodes)) #Vector of the distance for each node, will be updated after every step of the algorithm
  distance[which (init_node==nodes)]<-0 #Set the distance of the initial node to 0
  current_node <- init_node
  while (length(nodes) != 0) {
    neighbournodes<-v2[which (current_node==v1)]# The neighbouring nodes of the current node
    neighbourweights<-w[which (current_node==v1)]#The weights of the edges connecting the current node to the neighbouring nodes.
    neighbourdistance<-distance[neighbournodes]
    alt_distance<-distance[current_node] + neighbourweights #Alternative distance
    distance[neighbournodes]<-pmin(neighbourdistance,alt_distance)#Set the distance of a node to the minimum
    nodes <- nodes[nodes != current_node]#Remove the checked node
    #Now I want to choose the node with minium distance as the current node 
    current_node <- nodes[1]
    min_dist_node <- distance[current_node]
    for (node in nodes) {
      if(distance[node] < min_dist_node)
      {
        min_dist_node <- distance[node]
        current_node <- node
      }
    }
  }
  return(distance)
}

