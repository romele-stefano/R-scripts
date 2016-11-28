###############
#   NETWORK   #
###############

# Set margin
par(mar=c(0,0,5,0))

# Set vertices size 
V(network)$size=degree(network)/6
plot(network, vertex.label = ifelse(degree(network) >=50,
V(network)$name, NA), vertex.color = ifelse(degree(network)>=50, "green", "lightblue"),
vertex.label.color="red", main="#DataScience: Influencers Network (500 tweets)")



##################
#   COMPONENTS   #
##################

# Find clusters
g.components <- clusters(network)

# which is the largest component
ix <- which.max(g.components$csize)

# get the subgraph correspondent to just the giant component
g.giant <- induced.subgraph(network, which(g.components$membership == ix))

# Set margins
par(mar=c(0,0,5,0))

# Set vertices size
V(g.giant)$size=degree(g.giant)/5

# Plot only giant component
plot(g.giant, vertex.label = ifelse(degree(g.giant) >=25,
V(g.giant)$name, NA), vertex.color = ifelse(degree(g.giant)>=25, "green", "lightblue"),
vertex.label.color="red", main="Giant Component")



################################
#   GIANT COMPONENT CLUSTERS   #
################################

# Find edge betweenness 
com <- edge.betweenness.community(g.giant)
V(g.giant)$memb <- com$membership

# Plot giant components with communities highlighted
plot(com, g.giant, vertex.label = NA, vertex.color = ifelse(degree(g.giant)>=30, "green", "lightblue"),
vertex.label.color="red",vertex.size=3, main="Giant Component Clusters")

