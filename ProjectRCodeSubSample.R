##########################################################
####          Analysis of FLixster Network            ####
####                                                  ####
####                Gnana Teja Peddi                  ####
####                                                  ####
##########################################################

## Setting Working Directory 
getwd()
dir_path <-"C:/Users/GnanaTeja/Desktop/IDS 564/Final Project"

## Clearing everything out of memory
rm(list=ls()) 

## Load package
library(igraph)

## Reading edge and node data
el = read.csv("edges.csv", header=F)
nl = read.csv("nodes.csv", header=F)
colnames(el) <- c("Source","Target")
colnames(nl) <- c("Nodes")
# write.csv(el, "edges_gephi.csv", row.names=FALSE)
# write.csv(nl, "nodes_gephi.csv", row.names=FALSE)
nl1 = nl

class(el)
# ---
# [1] "data.frame"
# ---
# Describe the data frame

str(el)

## Creating the undirected graph object for Flixster network
g_Flixster = graph.data.frame(el, directed = FALSE, vertices= nl)

## Edges
ecount(g_Flixster)
# ---
# [1] 9197337
# ---

## Vertices
vcount(g_Flixster)
# ---
# [1] 2523386
# ---

## Generating an induced subgraph of the Flixster network
nl_subset = data.frame(V1 = nl[1:5000,])
g_Flixter_subgraph = induced_subgraph(g_Flixster, nl_subset[["V1"]])

## Edges
ecount(g_Flixter_subgraph)
# ---
# [1] 49009
# ---

## Vertices
vcount(g_Flixter_subgraph)
# ---
# [1] 5000
# ---

## Exporting the subgraph for use in Gephi
el_subset = data.frame(get.edgelist(g_Flixter_subgraph))
colnames(el_subset) <- c("Source","Target")
colnames(nl_subset) <- c("Nodes")
write.csv(el_subset, "edges_subgraph.csv", row.names=FALSE)
write.csv(nl_subset, "nodes_subgraph.csv", row.names=FALSE)

plot(g_Flixter_subgraph, layout=layout.fruchterman.reingold(g_Flixter_subgraph))
# write.csv(edge_frame_subset,file = "Subset_Edges.csv")

## Check whether Self_loops exist, as do multiple edges
is.simple(g_Flixter_subgraph)
#Is it a simple graph? No!
# ---
#[1] FALSE
# ---

## Simplifying the network graph
# Create edge weights
# E(g_Flixter_subgraph)$weight <- 1
# E(g_Flixter_subgraph)$weight 
g_Flixster_simpl = simplify(g_Flixter_subgraph) #, edge.attr.comb="sum"
is.simple(g_Flixster_simpl)
#Is it a simple graph? Yes!
# ---
#[1] TRUE
# ---

# Summarize the graph structure
summary(g_Flixster_simpl)

## Edges
ecount(g_Flixster_simpl)
# ---
# [1] 7918801
# ---

## Vertices
vcount(g_Flixster_simpl)
# ---
# [1] 2523386
# ---

############################
#### Graph connectivity ####
############################
# We can look at the connectivity of the graph 
# by seeing the neighbors of some selected nodes
neighbors(g_Flixster_simpl, v=c('900'))
neighbors(g_Flixster_simpl, v=c('592540'))


######################
#### Node Degrees ####
######################
# All Degrees
all.deg <- degree(g_Flixster_simpl, v=V(g_Flixster_simpl), mode="all")

# Overall Max Degree Node
max(all.deg)
# ---
# [1] 1474
# ---

V(g_Flixster_simpl)$name[degree(g_Flixster_simpl)==max(degree(g_Flixster_simpl))] # gives the corresponding user
# ---
# [1] "46246"
# ---

# Mean Degree
# table(degree(g_Flixster_simpl, mode="all"))
mean(degree(g_Flixster_simpl), mode="all")
# ---
# [1] 6.27633
# ---


##########################################
# Vertex Strength Distribution Histogram #
##########################################
# par(mfrow=c(2,1))
hist(graph.strength(g_Flixster_simpl, mode='all'), col="lightblue",
     xlab="Vertex Strength", ylab="Frequency", 
     main="Vertex Strength Distribution",
     breaks = 100)


###############################
# Log-log degree distribution #
###############################
# par(mfrow=c(1, 2))
d.net <- degree(g_Flixster_simpl, mode='all')
dd.net <- degree.distribution(g_Flixster_simpl, mode='all')
d <- 1:max(d.net)-1
ind <- (dd.net != 0)
plot(d[ind], dd.net[ind], log="xy", col="blue",
     xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution")


###############################################
# Average neighbor degree versus vertex degree#
###############################################
par(mfrow=c(1, 1))
d.net <- degree(g_Flixster_simpl, mode='all')
a.nn.deg <- graph.knn(g_Flixster_simpl,V(g_Flixster_simpl))$knn
plot(d.net, a.nn.deg, log="xy", 
     col="goldenrod", xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree"))


####################################
# Connectivity Measures of Network #
#################################### 
## Network Density
graph.density(g_Flixster_simpl)
# ---
## [1] 2.487266e-06
# ---

## Average Path Length
mean_distance(g_Flixster_simpl) #average.path.length(g_Flixster_simpl, directed=TRUE)
# ---
## [1] 2.487266e-06
# ---

## Number of weakly connected components
is.connected(g_Flixster_simpl, mode="weak")
# ---
## [1] TRUE
# ---
g_Flixster_simpl.wcc <- clusters(g_Flixster_simpl, mode="weak")
table(g_Flixster_simpl.wcc$csize)
# ---
## 2523386 
##       1
# ---

## Number of strongly connected components
is.connected(g_Flixster_simpl, mode="strong")
# ---
## [1] TRUE
# ---
g_Flixster_simpl.scc <- clusters(g_Flixster_simpl, mode="strong")
table(g_Flixster_simpl.scc$csize)
# ---
## 2523386 
##       1
# ---

# ########################
# # Reciprocal Relations #
# ########################
# reciprocity(g_Flixster_simpl)
# dyad_census(g_Flixster_simpl)
# which_mutual(g_Flixster_simpl)
# sum(which_mutual(g_Flixster_simpl))/2 == dyad_census(g_Flixster_simpl)$mut
# # Table of vertexes in mutual edges
# table(unlist(strsplit(attr(E(g_Flixster_simpl)[which_mutual(g_Flixster_simpl)],"vnames"),"\\|")))/2

####################
# Network Diameter #
####################
diameter(g_Flixster_simpl)
# ---
## [1] 
# ---

##############
# Clustering #
##############
# Global clustering coefficient
transitivity(g_Flixster_simpl)
# ---
## [1] 0.01365047
# --- 

#################
# Clique Census #
#################
# Clique structure: 5 cliques of size 5, 39 cliques of size 4, 335 triangles
table(sapply(cliques(g_Flixster_simpl), length))
table(sapply(maximal.cliques(g_Flixster_simpl), length))
cliques(g_Flixster_simpl)[sapply(cliques(g_Flixster_simpl), length) == 5]
clique.number(g_Flixster_simpl)


#A <- get.adjacency(g_Flixster_simpl, sparse=FALSE)

# Can try either of these weighting schemes for various measures; they change the interpretation of the measures
# Inverse weight
E(g_Flixster_simpl)$weight <- inv_weight
# Regular weight
E(g_Flixster_simpl)$weight <- num_weight


################################
# Local Measures: Centralities #
################################

# Embeddedness/ inverse of structural hole access (see Burt 2004)
constraints_SAP <- round(constraint(g_Flixster_simpl, nodes=V(g_Flixster_simpl)), digits=4)
# Degree centrality
degree_SAP <- degree(g_Flixster_simpl)
# Node betweenness
nodebetweens_SAP <- round(betweenness(g_Flixster_simpl, v=V(g_Flixster_simpl), directed = TRUE, nobigint =TRUE, normalized = FALSE))
# Edge betwenness
edgebetweens_SAP<-edge.betweenness(g_Flixster_simpl, e=E(g_Flixster_simpl), directed = TRUE)
# Local clustering coefficients
clustering_SAP <- transitivity(g_Flixster_simpl, type="local", vids=V(g_Flixster_simpl)) 

# Plots 1 and 2: Can run them together
par(mfrow=c(1, 2))
edge_frame<-data.frame(edgebetweens_SAP, num_weight, inv_weight)
a_edge<-aggregate(edgebetweens_SAP ~ inv_weight, data=edge_frame, mean)
plot(a_edge, col="blue", log="xy", xlab="Weight of edge", ylab="Average Betweenness of edges")
node_frame<-data.frame(nodebetweens_SAP, constraints_SAP, clustering_SAP, degree_SAP)
a_node<-aggregate(nodebetweens_SAP ~ clustering_SAP, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Clustering", ylab="Average Betweenness of nodes")

# Plot set 2: Four plots 
par(mfrow=c(2, 2))
a_node<-aggregate(nodebetweens_SAP ~ degree_SAP, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Betweenness of nodes")
a_edge<-aggregate(edgebetweens_SAP ~ num_weight, data=edge_frame, mean)
plot(a_edge, col="blue", log="xy", xlab="Weight of edge", ylab="Average Betweenness of edges")
a_node<-aggregate(clustering_SAP ~ degree_SAP, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Clustering")
a_node<-aggregate(constraints_SAP ~ degree_SAP, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Constraint (Embeddedness)")

# Closeness Centrality
close_SAP <- closeness(g_Flixster_simpl)
# Eigen Centrality
eig_SAP <- evcent(g_Flixster_simpl)$vector

# Hub and Authority Scores
hub_SAP <- hub.score(g_Flixster_simpl, weights=inv_weight)$vector
auth_SAP <- authority.score(g_Flixster_simpl, weights=inv_weight)$vector
head(sort(hub_SAP, decreasing=TRUE))
head(sort(auth_SAP, decreasing=TRUE))

centralities <- cbind(degree_SAP, nodebetweens_SAP, edgebetweens_SAP, close_SAP, eig_SAP, hub_SAP, auth_SAP)
cor.matrix = round(cor(centralities), 4)
write.csv(cor.matrix, "cor_matrix.csv")



community.significance.test <- function(graph, vs, ...) {
  if (is.directed(graph)) stop("This method requires an undirected graph")
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  # Total degree among nodes in the vs list, minus the degree within the subgraph 
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}


# Community detection using the Walktrap Algorithm
flixster_walk <- walktrap.community(g_overall_simple)
c.w <- membership(flixster_walk)
table(c.w, useNA = c("no"))

# Here, we are testing community significance for just two of the communities. 
v_comp1 <- V(g_overall_simple)[c.w==1]
v_comp2 <- V(g_overall_simple)[c.w==2]
v_comp3 <- V(g_overall_simple)[c.w==3]
v_comp4 <- V(g_overall_simple)[c.w==4]
v_comp5 <- V(g_overall_simple)[c.w==5]
v_comp6 <- V(g_overall_simple)[c.w==6]
v_comp7 <- V(g_overall_simple)[c.w==7]
v_comp8 <- V(g_overall_simple)[c.w==8]
v_comp9 <- V(g_overall_simple)[c.w==9]
v_comp10 <- V(g_overall_simple)[c.w==10]
v_comp11 <- V(g_overall_simple)[c.w==11]
v_comp12 <- V(g_overall_simple)[c.w==12]
v_comp13 <- V(g_overall_simple)[c.w==13]
v_comp14 <- V(g_overall_simple)[c.w==14]
v_comp15 <- V(g_overall_simple)[c.w==15]
v_comp16 <- V(g_overall_simple)[c.w==16]
v_comp17 <- V(g_overall_simple)[c.w==17]


community.significance.test(g_overall_simple, v_comp1)
community.significance.test(g_overall_simple, v_comp2)
community.significance.test(g_overall_simple, v_comp3)
community.significance.test(g_overall_simple, v_comp4)
community.significance.test(g_overall_simple, v_comp5)
community.significance.test(g_overall_simple, v_comp6)
community.significance.test(g_overall_simple, v_comp7)
community.significance.test(g_overall_simple, v_comp8)
community.significance.test(g_overall_simple, v_comp9)
community.significance.test(g_overall_simple, v_comp10)
community.significance.test(g_overall_simple, v_comp11)
community.significance.test(g_overall_simple, v_comp12)
community.significance.test(g_overall_simple, v_comp13)
community.significance.test(g_overall_simple, v_comp14)
community.significance.test(g_overall_simple, v_comp15)
#community.significance.test(graf, v_comp16)
#community.significance.test(graf, v_comp17)

plot(flixster_walk,g_overall_simple, vertex.label = NA, vertex.size=2)


sub_comp1 <-induced.subgraph(g_overall_simple, v= c('111',   '117', 	'139', 	'165', 	'168', 	'192', 	'30', 	'41', 	'424', 	'426', 	'427', 	'428', 	'430', 	'433', 	'434', 	'436', 	'439', 	'440', 	'441', 	'444', 	'445', 	'450', 	'452', 	'457', 	'460', 	'461', 	'462', 	'464', 	'466', 	'468', 	'471', 	'473', 	'481', 	'483', 	'484', 	'485', 	'487', 	'488', 	'491', 	'492', 	'493', 	'495', 	'496', 	'497', 	'503', 	'505', 	'506', 	'507', 	'5', 	'511', 	'512', 	'513', 	'518', 	'519', 	'523', 	'524', 	'525', 	'528', 	'530', 	'534', 	'535', 	'538', 	'539', 	'540', 	'542', 	'543', 	'544', 	'545', 	'546', 	'547', 	'548', 	'550', 	'554', 	'555', 	'556', 	'557', 	'561', 	'564', 	'566', 	'567', 	'571', 	'573', 	'574', 	'575', 	'576', 	'577', 	'578', 	'579', 	'580', 	'582', 	'583', 	'584', 	'585', 	'587', 	'588', 	'590', 	'593', 	'595', 	'600', 	'601', 	'602', 	'603', 	'605', 	'606', 	'607', 	'610', 	'611', 	'614', 	'616', 	'619', 	'620', 	'621', 	'622', 	'627', 	'628', 	'629', 	'637', 	'640', 	'641', 	'642', 	'644', 	'645', 	'646', 	'647', 	'648', 	'653', 	'655', 	'659', 	'661', 	'775', 	'781', 	'811', 	'971'))
sub_comp2 <- induced.subgraph(g_overall_simple, v= c('103', 	'182', 	'183', 	'185', 	'186', 	'187', 	'188', 	'189', 	'191', 	'194', 	'196', 	'197', 	'200', 	'201', 	'202', 	'203', 	'205', 	'206', 	'207', 	'208', 	'209', 	'210', 	'211', 	'212', 	'213', 	'214', 	'218', 	'220', 	'221', 	'227', 	'228', 	'229', 	'230', 	'231', 	'232', 	'233', 	'234', 	'237', 	'240', 	'241', 	'243', 	'244', 	'246', 	'248', 	'249', 	'250', 	'251', 	'252', 	'254', 	'255', 	'256', 	'258', 	'262', 	'263', 	'264', 	'265', 	'268', 	'272', 	'273', 	'276', 	'277', 	'278', 	'281', 	'285', 	'286', 	'287', 	'289', 	'290', 	'292', 	'293', 	'294', 	'296', 	'298', 	'300', 	'302', 	'304', 	'305', 	'313', 	'314', 	'317', 	'318', 	'319', 	'32', 	'321', 	'322', 	'324', 	'325', 	'326', 	'329', 	'330', 	'332', 	'333', 	'334', 	'335', 	'336', 	'337', 	'338', 	'340', 	'341', 	'342', 	'343', 	'344', 	'382', 	'4', 	'482', 	'516', 	'527', 	'609', 	'623', 	'849', 	'884', 	'947'))
sub_comp3 <- induced.subgraph(g_overall_simple, v= c('13', 	'581', 	'717', 	'719', 	'721', 	'723', 	'726', 	'728', 	'729', 	'730', 	'731', 	'732', 	'733', 	'734', 	'739', 	'746', 	'749', 	'750', 	'752', 	'753', 	'755', 	'760', 	'762', 	'765', 	'766', 	'768', 	'769', 	'770', 	'772', 	'773', 	'774', 	'791', 	'793', 	'795', 	'796', 	'797', 	'798', 	'799', 	'802', 	'803', 	'804', 	'805', 	'807', 	'808', 	'809', 	'810', 	'812', 	'813', 	'814', 	'816', 	'817', 	'818', 	'819', 	'820', 	'821', 	'822', 	'823', 	'824', 	'825', 	'828', 	'829', 	'830', 	'833', 	'834', 	'835', 	'836', 	'837', 	'838', 	'839', 	'841', 	'842', 	'843', 	'845', 	'846', 	'848', 	'850', 	'852', 	'853', 	'854', 	'855', 	'856', 	'857', 	'858', 	'859', 	'861', 	'863', 	'864', 	'868', 	'870', 	'871', 	'872', 	'873', 	'878', 	'879', 	'880', 	'882'))
sub_comp4 <- induced.subgraph(g_overall_simple, v= c('11', 	'377', 	'383', 	'385', 	'389', 	'391', 	'394', 	'396', 	'398', 	'399', 	'400', 	'401', 	'402', 	'403', 	'405', 	'408', 	'409', 	'410', 	'411', 	'412', 	'413', 	'415', 	'416', 	'417', 	'418', 	'419', 	'421', 	'422', 	'423', 	'44'))
sub_comp5 <- induced.subgraph(g_overall_simple, v= c('12', 	'23', 	'345', 	'346', 	'347', 	'348', 	'350', 	'351', 	'352', 	'353', 	'354', 	'355', 	'356', 	'357', 	'358', 	'359', 	'360', 	'361', 	'362', 	'363', 	'364', 	'366', 	'367', 	'368', 	'369', 	'370', 	'371', 	'372', 	'373', 	'376', 	'378', 	'379', 	'380', 	'381', 	'384', 	'386', 	'387', 	'388', 	'390', 	'970'))
sub_comp6 <- induced.subgraph(g_overall_simple, v= c('100', 	'101', 	'102', 	'104', 	'105', 	'106', 	'107', 	'108', 	'110', 	'112', 	'114', 	'115', 	'116', 	'118', 	'119', 	'120', 	'121', 	'123', 	'124', 	'125', 	'126', 	'127', 	'128', 	'129', 	'132', 	'2', 	'223', 	'349', 	'499', 	'565', 	'59', 	'60', 	'61', 	'612', 	'64', 	'65', 	'67', 	'68', 	'69', 	'70', 	'71', 	'72', 	'73', 	'74', 	'75', 	'76', 	'77', 	'78', 	'79', 	'80', 	'82', 	'83', 	'84', 	'85', 	'86', 	'887', 	'89', 	'91', 	'92', 	'93', 	'95', 	'96', 	'97', 	'975', 	'98', 	'99'))

plot(sub_comp1, layout=layout.lgl,vertex.size = 6)
plot(sub_comp2, layout=layout.lgl,vertex.size = 6)
plot(sub_comp3, layout=layout.lgl,vertex.size = 6)
plot(sub_comp4, layout=layout.lgl,vertex.size = 6)
plot(sub_comp5, layout=layout.lgl,vertex.size = 6)
plot(sub_comp6, layout=layout.lgl,vertex.size = 6)

################


# This one implements stats for susceptible as well as remove
# Also, make use of the susceptible again feature
# Please note that the only required argument for this function is the graph object. The other arguments can be overriden or skipped by the calling function; if skipped,
# the default values are used.
# Arguments: 1) network.i: graph object (required), 2) simlength: number of iterations (rounds) for the simulation, 3) p.t: probability of infecting a connected susceptible neighbor, if a node is infected
# 4) display_net: show the evolving network plots for each round of the simulation (press Enter to continue, q to quit). Set it to FALSE if you have a lot of 
# rounds and just want to collect the summary timestats. 5) removeafter: Number of rounds that an infected node can infect neigbors (infectious), 
# after which it moves to a Removed state where it is immune and not infectious. 5) susceptibleafter: Number of rounds after which a node in the Removed state becomes susceptible to infection again

simulate_sir <- function(network.i, simlength=15, p.t=0.2, display_net=TRUE, removeafter=2, susceptibleafter=10000) 
{
  
  links <-get.edgelist(network.i)
  N<- vcount(network.i)
  time_stats<-list()
  
  # Initialize time stats. 
  # Number of nodes in S, I, or R status in each round of time
  time_stats$infected_t<-rep(1,simlength)
  time_stats$removed_t<-rep(0,simlength)
  #susceptible is total that are not removed or infected
  time_stats$susceptible_t<-rep(N-1,simlength)
  time_stats$time_iter<-rep(0,simlength)
  time_stats$inft<-rep(0,simlength)
  
  infected <- logical(N) # initialize infection status
  #   susceptible <- rep(TRUE, N) # initialize susceptible status
  #   removed<-logical(N)
  egen_Cen<-evcent(network.i)
  high_eigen<-order(-egen_Cen$vector)[1]
  patientzero <- high_eigen # select 'patient zero'
  
  # Initialize a vector that keeps track of the time of infection for each node.
  infected_time<-rep(0, N)
  
  
  #patient zero  
  infected[patientzero] <- TRUE
  
  
  # Used to count towards a removal; after a certain number of periods, the node will be immune (i.e. removed)
  infected_time[patientzero] <- 1
  
  if (N > 50) {
    V(network.i)$size <- 2
    V(network.i)$label <- ""
  }
  if (display_net) {
    
    fixlayout <- layout.kamada.kawai(network.i)  # store a fixed layout for the graph
    node.colour <- rep("SkyBlue2",N) # initialize node colours (SkyBlue2 is also the default node colour in igraph)
    node.colour[patientzero] <- "red" # infected nodes will be coloured red
    plot(network.i,layout=fixlayout, main="Time = 0", vertex.color=node.colour)
  }
  for (i in 1:simlength) {
    
    
    discordant.links <- which(xor(infected[as.integer(links[,1])],infected[as.integer(links[,2])])) # find the indeces of links that connect an infected individual to an uninfected
    
    transmit <- rbinom(length(discordant.links),1,p.t) # determine randomly which of the discordant links transmit the disease
    
    # let me update the infection vector in three steps to make it easier to read:
    transmitter.links <- discordant.links[transmit==1]
    nodes.of.transmitter.links <- unique(as.vector(as.integer(links[transmitter.links,1:2]))) # gets both nodes of the transmitter links into a single vector; unique just filters out repetitions
    infected[nodes.of.transmitter.links] <- TRUE # here I simply set both nodes to TRUE (although the transmitter already had 'TRUE'). In more complex models, you might want to do a further check here and overwrite only the newly infected nodes.
    
    
    
    if (display_net) {
      node.colour[infected] <- "red"
      # Make the removed points as yellow, susceptible points as skyblue. Uncomment these for advanced lab.
      #       node.colour[removed] <- "yellow"
      #       node.colour[susceptible] <-"SkyBlue2"
      input<-readline() # waits for the user to press <ENTER> before proceeding; you need to switch to the console to do this
      infection_rate<-((table(infected)["TRUE"])/length(infected))*100
      time_stats$time_iter[i]<-i
      time_stats$inft[i]<-infection_rate
      # TIP: Hit q to break the simulation between renderings of the network
      if (input=="q") {break}
      plot(network.i,layout=fixlayout, main=paste("Time =", i, " out of ", simlength," Propagation Rate:",infection_rate,"%"), vertex.color=node.colour)
    }
    
    
  }
  return(time_stats)
}


V(sub_comp1)$name<-as.integer(1:vcount(sub_comp1))
V(sub_comp2)$name<-as.integer(1:vcount(sub_comp2))
V(sub_comp3)$name<-as.integer(1:vcount(sub_comp3))
V(sub_comp4)$name<-as.integer(1:vcount(sub_comp4))
V(sub_comp5)$name<-as.integer(1:vcount(sub_comp5))
V(sub_comp6)$name<-as.integer(1:vcount(sub_comp6))


prefer_graf<-list()
prefer_graf$ba_sub1 <- barabasi.game(133, power=0.98, m=1,  directed=FALSE)
V(prefer_graf$ba_sub1)$name<-as.integer(1:vcount(prefer_graf$ba_sub1))
prefer_graf$ba_sub2 <- barabasi.game(112, power=0.98, m=1,  directed=FALSE)
V(prefer_graf$ba_sub2)$name<-as.integer(1:vcount(prefer_graf$ba_sub2))
prefer_graf$ba_sub3 <- barabasi.game(96, power=0.98, m=1,  directed=FALSE)
V(prefer_graf$ba_sub3)$name<-as.integer(1:vcount(prefer_graf$ba_sub3))
prefer_graf$ba_sub4 <- barabasi.game(30, power=0.98, m=1,  directed=FALSE)
V(prefer_graf$ba_sub4)$name<-as.integer(1:vcount(prefer_graf$ba_sub4))
prefer_graf$ba_sub5 <- barabasi.game(40, power=0.98, m=1,  directed=FALSE)
V(prefer_graf$ba_sub5)$name<-as.integer(1:vcount(prefer_graf$ba_sub5))
prefer_graf$ba_sub6 <- barabasi.game(66, power=0.98, m=1,  directed=FALSE)
V(prefer_graf$ba_sub6)$name<-as.integer(1:vcount(prefer_graf$ba_sub6))

par(mfrow=c(1,1))
# When you view the simulations of evolving network plots, you can press Enter to continue each step, or press q to quit the simulation o
# See the list of arguments above in the function definition
infected_school_tm1<-simulate_sir(sub_comp1, simlength= 100,  p.t=0.05)
plot(infected_school_tm1$time_iter,infected_school_tm1$inft,xlab="Time Iterations",ylab="Infection Rate")
infected_school_tm1_ba1<-simulate_sir(prefer_graf$ba_sub1, simlength= 100,  p.t=0.05)
plot(infected_school_tm1_ba1$time_iter, infected_school_tm1_ba1$inft,xlab="Time Iterations",ylab="Infection Rate")


require(ggplot2)
df_1<-data.frame(1:100,infected_school_tm1$inft,infected_school_tm1_ba1$inft)
ggplot(df_1, aes(1:100)) +                    # basic graphical object
  geom_line(aes(y=infected_school_tm1$inft,colour="Flixster")) +  # first layer
  geom_line(aes(y=infected_school_tm1_ba1$inft,colour="Barabasi")) + # second layer
  xlab("Time Iterations") +
  ylab("Propagation Rate") +
  scale_colour_manual(name="Network",
                      values=c(Flixster="red", Barabasi="blue"))


infected_school_tm2<-simulate_sir(sub_comp2, simlength= 100,  p.t=0.05)
plot(infected_school_tm2$time_iter,infected_school_tm2$inft,xlab="Time Iterations",ylab="Infection Rate")
infected_school_tm2_ba2<-simulate_sir(prefer_graf$ba_sub2, simlength= 100,  p.t=0.05)
plot(infected_school_tm2_ba2$time_iter, infected_school_tm2_ba2$inft,xlab="Time Iterations",ylab="Infection Rate")

df_2<-data.frame(1:100,infected_school_tm2$inft,infected_school_tm2_ba2$inft)
ggplot(df_2, aes(1:100)) +                    # basic graphical object
  geom_line(aes(y=infected_school_tm2$inft,colour="Flixster")) +  # first layer
  geom_line(aes(y=infected_school_tm2_ba2$inft,colour="Barabasi")) +  # second layer
  xlab("Time Iterations") +
  ylab("Propagation Rate") +
  scale_colour_manual(name="Network",
                      values=c(Flixster="red", Barabasi="blue"))

infected_school_tm3<-simulate_sir(sub_comp3, simlength= 100,  p.t=0.05)
plot(infected_school_tm3$time_iter,infected_school_tm3$inft,xlab="Time Iterations",ylab="Infection Rate")
infected_school_tm3_ba3<-simulate_sir(prefer_graf$ba_sub3, simlength= 100,  p.t=0.05)
plot(infected_school_tm3_ba3$time_iter, infected_school_tm3_ba3$inft,xlab="Time Iterations",ylab="Infection Rate")

df_3<-data.frame(1:100,infected_school_tm3$inft,infected_school_tm3_ba3$inft)
ggplot(df_3, aes(1:100)) +                    # basic graphical object
  geom_line(aes(y=infected_school_tm3$inft,colour="Flixster")) +  # first layer
  geom_line(aes(y=infected_school_tm3_ba3$inft,colour="Barabasi")) + # second layer
  
  xlab("Time Iterations") +
  ylab("Propagation Rate") +
  scale_colour_manual(name="Network",
                      values=c(Flixster="red", Barabasi="blue"))


infected_school_tm4<-simulate_sir(sub_comp4, simlength= 100,  p.t=0.05)
plot(infected_school_tm4$time_iter,infected_school_tm4$inft,xlab="Time Iterations",ylab="Infection Rate")
infected_school_tm4_ba4<-simulate_sir( prefer_graf$ba_sub4, simlength= 100,  p.t=0.05)
plot(infected_school_tm4_ba4$time_iter, infected_school_tm4_ba4$inft,xlab="Time Iterations",ylab="Infection Rate")

df_4<-data.frame(1:100,infected_school_tm4$inft,infected_school_tm4_ba4$inft)
ggplot(df_4, aes(1:100)) +                    # basic graphical object
  geom_line(aes(y=infected_school_tm4$inft,colour="Flixster")) +  # first layer
  geom_line(aes(y=infected_school_tm4_ba4$inft,colour="Barabasi")) +# second layer
  
  xlab("Time Iterations") +
  ylab("Propagation Rate") +
  scale_colour_manual(name="Network",
                      values=c(Flixster="red", Barabasi="blue"))


infected_school_tm5<-simulate_sir(sub_comp5, simlength= 100,  p.t=0.05)
plot(infected_school_tm5$time_iter,infected_school_tm5$inft,xlab="Time Iterations",ylab="Infection Rate")
infected_school_tm5_ba5<-simulate_sir(prefer_graf$ba_sub5, simlength= 100,  p.t=0.05)
plot(infected_school_tm5_ba5$time_iter, infected_school_tm5_ba5$inft,xlab="Time Iterations",ylab="Infection Rate")

df_5<-data.frame(1:100,infected_school_tm5$inft,infected_school_tm5_ba5$inft)
ggplot(df_5, aes(1:100)) +                    # basic graphical object
  geom_line(aes(y=infected_school_tm5$inft,colour="Flixster")) +  # first layer
  geom_line(aes(y=infected_school_tm5_ba5$inft,colour="Barabasi")) +# second layer
  xlab("Time Iterations") +
  ylab("Propagation Rate") +
  scale_colour_manual(name="Network",
                      values=c(Flixster="red", Barabasi="blue"))

infected_school_tm6<-simulate_sir(sub_comp6, simlength= 100,  p.t=0.05)
plot(infected_school_tm6$time_iter,infected_school_tm6$inft,xlab="Time Iterations",ylab="Infection Rate")
infected_school_tm6_ba6<-simulate_sir(prefer_graf$ba_sub6, simlength= 100,  p.t=0.05)
plot(infected_school_tm6_ba6$time_iter, infected_school_tm6_ba6$inft,xlab="Time Iterations",ylab="Infection Rate")

df_6<-data.frame(1:100,infected_school_tm6$inft,infected_school_tm6_ba6$inft)
ggplot(df_6, aes(1:100)) +                    # basic graphical object
  geom_line(aes(y=infected_school_tm6$inft,colour="Flixster")) +  # first layer
  geom_line(aes(y=infected_school_tm6_ba6$inft,colour="Barabasi")) + # second layer
  xlab("Time Iterations") +
  ylab("Propagation Rate") +
  scale_colour_manual(name="Network",
                      values=c(Flixster="red", Barabasi="blue"))
