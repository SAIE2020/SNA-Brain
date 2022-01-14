setwd("C:/Users/sarah/SNA")

edges <- read.csv("brain connections (edge).csv")
nodes<- read.csv("brain structures (node).csv")
View(edges)
View(nodes)
install.packages("igraph")
install.packages("Rtools")
install.packages("sna")
library(igraph)
brain_graph <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
brain_graph<- set.vertex.attribute(brain_graph, "name", value=c(nodes$Label))
plot(brain_graph)
#Actor Centrality Measures
degree(brain_graph, v=V(brain_graph),mode="in")
degree(brain_graph, v=V(brain_graph), mode="out")
in_closeness<-closeness(brain_graph, v=V(brain_graph), mode="in")
max(in_closeness)
out_closeness<-closeness(brain_graph, vids=V(brain_graph), mode="out")
out_closeness
max(out_closeness)
betweenness(brain_graph, v=V(brain_graph), directed=TRUE)
eigen_centrality(brain_graph, directed = TRUE)
#graph is acyclic so eigenvalues are all zero
test<-subcomponent(brain_graph, 1, mode = c("out"))
length(test)
V(brain_graph)
#build loop for hierarchical closeness
graph<-brain_graph
x<-seq(from= 1, to=length(V(graph)), by=1)
#loop for out degree
for(i in x ){
  test<-subcomponent(graph, i, mode = "out")
  len<-length(test)
  close<-suppressWarnings(closeness(graph, vids=i, mode="out", normalized = TRUE))
  hc_value<-len+close
  print(hc_value)
}
#loop for in degree
for(i in x ){
  test<-subcomponent(graph, i, mode = "in")
  len<-length(test)
  close<-suppressWarnings(closeness(graph, vids=i, mode="in", normalized = TRUE))
  hc_value<-len+close
  print(hc_value)
}
#Network Measures
edge_density(brain_graph, loops=FALSE)
#determine the level of connected the graph is
y<-seq(from= 1, to=length(V(brain_graph)), by=1)
for(i in y){
  print(dfs(brain_graph, root=i, neimode = "in", unreachable = FALSE))
}
y<-seq(from= 1, to=length(V(brain_graph)), by=1)
for(i in y){
  print(dfs(brain_graph, root=i, neimode = "out", unreachable = FALSE))
}
#this graph is not strongly connected, indicating that unconnected should be set to TRUE 
mean_distance(brain_graph, directed = TRUE, unconnected = TRUE)
diameter(brain_graph, directed = TRUE, unconnected = TRUE, weights = NULL)
centralization.degree(brain_graph, mode= "in")$centralization
centralization.degree(brain_graph, mode= "out")$centralization
#compute global clustering coefficient
install.packages("DirectedClustering")
library(DirectedClustering)
adj_brain<-get.adjacency(brain_graph,names=TRUE)
typeof(adj_brain)
adj_brain<-data.matrix(adj_brain, rownames.force = NA)
typeof(adj_brain)
#adj_brain needed to be converted from an s4 objrct into a matrix
ClustF(adj_brain, type = "directed", isolates = "zero", norm=1)
ClustBCG(adj_brain, type = "directed", isolates = "zero")
#no difference between $GlobaltotalCC between the two algorithms
#Assign modularity classes to node list
nodes$Modularity_Class=nodes$Purpose
nodes["Modularity_Class"][nodes["Modularity_Class"] == "Auditory Processing"] <- 1
nodes["Modularity_Class"][nodes["Modularity_Class"] == "Sensory Information"] <- 2
nodes["Modularity_Class"][nodes["Modularity_Class"] == "Sensory Processing"] <- 2
nodes["Purpose"][nodes["Purpose"]=="Sensory Processing"]<-"Sensory Information"
nodes["Modularity_Class"][nodes["Modularity_Class"] == "Visual Processing"] <- 3
nodes["Modularity_Class"][nodes["Modularity_Class"] == "Speech Production"] <- 4
nodes["Modularity_Class"][nodes["Modularity_Class"] == "Attention"] <- 5
nodes["Modularity_Class"][nodes["Modularity_Class"] == "Working Memory"] <- 6
brain_graph <- set_vertex_attr(brain_graph, "attribute", value = as.factor(nodes$Modularity_Class))
#change graph to undirected to run modularity analysis
brain_graph_und<-as.undirected(brain_graph,mode = "collapse",edge.attr.comb = igraph_opt("edge.attr.comb"))
modularity(brain_graph_und, V(brain_graph_und)$attribute)
#graph components, using directed version of graph
components(brain_graph, mode ="weak")
components(graph, mode = c("weak", "strong"))
#all components are connected, there are no nodes that are disconnected 