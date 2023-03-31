#install.packages("GGally")
#install.packages("network")
#install.packages("sna")
#install.packages("intergraph")
library(intergraph)
library(GGally)
library(network)
library(sna)
library(readr)
library(ggplot2)
library(igraph)

gnet <- rgraph(10, mode = "graph", tprob = 0.5) 
gnet <- network(gnet, directed = FALSE)
network.vertex.names(gnet) = letters[1:10]
ggnet2(gnet)
ggnet2(gnet, node.size = 6, node.color = "black", edge.size = 1, edge.color = "grey")
ggnet2(gnet, size = 6, color = rep(c("tomato", "steelblue"), 5))
 


# Create sample data
data <- data.frame(matrix(ncol = 2, nrow = 200))
names(data) <- c("first", "second")
for(x in 1:200) {
  a <- c("A", "B", "C", "D", "E")
  bc <- paste(sample(a,1, replace = T), sample(a,1, replace = T), sep = "")
  de <- paste(sample(a,1, replace = T), sample(a,1, replace = T), sep = "")
  ifelse(bc == de, data[x,1] <- paste(sample(a,1, replace = T), sample(a,1, replace = T), sep = ""), data[x,1] <- bc)
  data[x,2] <- paste(de, sep = "")
}

friend <- data.frame(unique(data$first))
for(x in 1:nrow(friend)) {
  g <- c("Female", "Male")
  friend[x,2] <- sample(g,1, replace = T)
}

names(friend) <- c("name", "gender")


# Create Network
net <- graph_from_data_frame(d = data, vertices = friend, directed = T)
plot(net, vertex.size = 2, edge.arrow.size = 0.1, vertex.label.cex = 0.8)
V(net)
E(net)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

# Histogram of node degree
hist(V(net)$degree, 
     col = "tomato2", 
     main = "Histogram of Node Degree",
     ylab = "Frequency",
     xlab = "Degree of Vertices")

# Color 
V(net)$color <- ifelse(V(net)$gender == "Male", "lightblue", "pink")

# Network diagram
set.seed(222)
plot(net, 
     vertext.size = 3,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.8)
legend("topright", c("Male","Female"), pch=21,
       col="#777777", pt.bg=c("lightblue","pink"), pt.cex=2, cex=.8)
title(main = "Friend Group")
# Rainbow color and 
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree, 
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)
title(main = "Friend Group")

# Hub and authorities
hs <- hub_score(net)$vector
as <- authority.score(net)$vector
par(mfrow = c(1,2))
set.seed(123)
plot(net, vertex.size = hs*30,
     main = "Hubs", vertex.color = rainbow(52), edge.arrow.size = 0.1, layout = layout.kamada.kawai)
set.seed(123)
plot(net, vertex.size = as*30,
     main = "Authorities", vertex.color = rainbow(52), edge.arrow.size = 0.1, layout = layout.kamada.kawai)



source("https://goo.gl/q1JFih")
x = cut_number(as.integer(net %v% "year"), 4)
net <- graph.data.frame(net, directed = T)
col = c("#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2")
names(col) = levels(x)
ggnet2(net, color = x, color.legend = "period", palette = col,
       edge.alpha = 1/4, edge.size = "weight",
       size = "outdegree", max_size = 4, size.cut = 3,
       legend.size = 12, legend.position = "bottom") +
  coord_equal()


