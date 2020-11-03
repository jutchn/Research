install.packages("igraph")
library(igraph)

#import freesurfer, make sure to use stringsAsFactors=FALSE yz 06/05/2020
freesurfer <- read.csv("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/freesurfer.csv", stringsAsFactors = FALSE)
group <- readRDS("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/MSN/hcp.dt1.rds")

subjects <- group[,1]

freesurferTrim <- freesurfer[freesurfer[, 1] %in% subjects, ]
freesurferTrim_group <- cbind(group,freesurferTrim)[,-6:-7]

freesurfer_binge <- freesurferTrim_group[freesurferTrim_group[, 5] == 1, ]
freesurfer_nobinge <- freesurferTrim_group[freesurferTrim_group[, 5] == 0, ] 
freesurfer_alc <- freesurferTrim_group[freesurferTrim_group[, 4] == 1, ]
freesurfer_noalc<- freesurferTrim_group[freesurferTrim_group[, 4] == 0, ]

#parse 7 features
parseFeatures <- function(freesurfer){
  thickness <- freesurfer[, grep("_Thck$" , colnames(freesurfer))]
  area <- freesurfer[, grep("_Area", colnames(freesurfer))]
  grayVol <- freesurfer[, grep("_GrayVol", colnames(freesurfer))]
  foldingIndex <- freesurfer[, grep("_FoldInd" , colnames(freesurfer))]
  curveIndex <- freesurfer[, grep("_CurvInd" , colnames(freesurfer))]
  gausCurv <- freesurfer[, grep("_GausCurv" , colnames(freesurfer))]
  meanCurv <- freesurfer[, grep("_MeanCurv" , colnames(freesurfer))]
  
  features <- list(thickness,area,grayVol,foldingIndex,curveIndex,gausCurv,meanCurv)
  return(features)
}

features <- parseFeatures(freesurferTrim_group)

feat_binge <- parseFeatures(freesurfer_binge)
feat_nobinge <- parseFeatures(freesurfer_nobinge)
feat_alc <- parseFeatures(freesurfer_alc)
feat_noalc <- parseFeatures(freesurfer_noalc)




# extract ROI names yz 06/05/2020
#roi.nm<-substr(colnames(thickness), start = 4, stop=nchar(colnames(thickness))-5) #implemented line 53

# build subject level brain network
#sub.net<-list() #implemented line 67

# for each subject do the following
# create a 68*7 matrix
# z-score each of 68 7*1 vector at individual level
# calculate pair-wise person correlation to produce 68*68 matrix for each subject


# now you have a list of individual level brain network

# for each brain network, calculate the following topological measures
# at node level: local efficiency, node degree, clustering coefficient, and characteristic path lengths
# at global level: global efficiency, Rich club coefficient
# you might need to binarize the data as they suggested, looking at the top 2%, 4%, and 6% connections, respectively



# 06/05/2020



#sorting region names alphabetically and assign region names

formatFeatures <- function(features){
  for(i in 1:7){
    features[[i]] <- features[[i]][,sort(names(features[[i]]))]
  }
  roi.nm <- substr(colnames(features[[1]]), start = 4, stop=nchar(colnames(features[[1]]))-5)
  for(i in 1:7){
    colnames(features[[i]]) <- roi.nm
  }
  return(features)
}

formattedFeatures <- formatFeatures(features)
#extracing names
roi.nm<-colnames(formattedFeatures[[1]])

ff_b   <- formatFeatures(feat_binge)
ff_nb  <- formatFeatures(feat_nobinge)
ff_al  <- formatFeatures(feat_alc)
ff_nal <- formatFeatures(feat_noalc)



#build 2 subject level brain network, 4 features and 7 features
buildNet <- function(features,featSelect){
  sub.net <- list()
  for(i in 1:nrow(features[[1]])){
    sub.net[[i]] <- features[[featSelect[1]]][i,]
    if(length(features)>1){
      for(j in 2:length(featSelect)){
        sub.net[[i]] <- rbind(sub.net[[i]], features[[featSelect[j]]][i,])
      }
    }
  }
  return(sub.net)
}

featSelect <- c(1:7)
sub.net.7 <- buildNet(formattedFeatures, featSelect)

sub.net.b.7   <- buildNet(ff_b, featSelect)
sub.net.nb.7  <- buildNet(ff_nb, featSelect)
sub.net.al.7  <- buildNet(ff_al, featSelect)
sub.net.nal.7 <- buildNet(ff_nal, featSelect)


featSelect <- c(1,2,3,7)
sub.net.4 <- buildNet(formattedFeatures, featSelect)

sub.net.b.4   <- buildNet(ff_b, featSelect)
sub.net.nb.4  <- buildNet(ff_nb, featSelect)
sub.net.al.4  <- buildNet(ff_al, featSelect)
sub.net.nal.4 <- buildNet(ff_nal, featSelect)


#z-scoring per subject
zscore <- function(df){
  df_num <- list()
  for(i in 1:length(df)){
    for(j in 1:nrow(df[[1]])){
      df_num[[i]] <- as.numeric(unlist(df[[i]][j,]))
      df[[i]][j,] <- (df[[i]][j,] - mean(df_num[[i]]))/sd(df_num[[i]])
    }
  }
  return(df)
}

sub.net.7.z <- zscore(sub.net.7)
sub.net.4.z <- zscore(sub.net.4)

sub.net.b.7.z   <- zscore(sub.net.b.7)
sub.net.nb.7.z  <- zscore(sub.net.nb.7)
sub.net.al.7.z  <- zscore(sub.net.al.7)
sub.net.nal.7.z <- zscore(sub.net.nal.7)

sub.net.b.4.z   <- zscore(sub.net.b.4)
sub.net.nb.4.z  <- zscore(sub.net.nb.4)
sub.net.al.4.z  <- zscore(sub.net.al.4)
sub.net.nal.4.z <- zscore(sub.net.nal.4)

#calculating pearson correlation for each subject
pearson_matrix <- function(df){
  df_cor <- list()
  for(i in 1:length(df)){
    df_cor[[i]] <- cor(df[[i]], use="complete.obs", method = "pearson")
  }
  return(df_cor)
}

sub.net.7.c     <- pearson_matrix(sub.net.7.z)
sub.net.4.c     <- pearson_matrix(sub.net.4.z)

sub.net.b.7.c   <- pearson_matrix(sub.net.b.7.z)
sub.net.nb.7.c  <- pearson_matrix(sub.net.nb.7.z)
sub.net.al.7.c  <- pearson_matrix(sub.net.al.7.z)
sub.net.nal.7.c <- pearson_matrix(sub.net.nal.7.z)

sub.net.b.4.c   <- pearson_matrix(sub.net.b.4.z)
sub.net.nb.4.c  <- pearson_matrix(sub.net.nb.4.z)
sub.net.al.4.c  <- pearson_matrix(sub.net.al.4.z)
sub.net.nal.4.c <- pearson_matrix(sub.net.nal.4.z)


#thresholding, creating binary/weighted matrices
threshold <- function(df,thresh_num,binary){
  for(i in 1:length(df[1,])){
    for(j in 1:length(df[,1])){
      if(df[i,j] < thresh_num){
        df[i,j] <- df[i,j] * 0
      } else{
        if(binary == 1){
          df[i,j] <- 1
        } else {
          df[i,j] <- df[i,j]
        }
      }
    }
  }
  return(df)
}

#formatting adj matrix, applying threshold and removing diagonals
formatAdjMatrix <- function(df, percent, binary){
  numSubjects <- length(df)
  numRegions <- nrow(df[[1]])
  
  df_format <- df
  top_num <- as.integer(percent * .01 * numRegions * (numRegions-1))
  if(top_num %% 2==1){
    top_num <- top_num + 1
  }
  for(i in 1:numSubjects){
    thresh_num <- sort(df[[i]], TRUE)[numRegions + top_num]
    df_format[[i]] <- threshold(df[[i]],thresh_num, binary)
  }
  for(i in 1:numSubjects){
    diag(df_format[[i]]) <- 0
  }
  
  return(df_format)
}
  
avgMat <- function(df){
  avg <- list()
  df_size <- ncol(df[[1]])
  sum <- matrix(0L, nrow = df_size, ncol = df_size)
  
  for(i in 1:length(df)){
    sum <- sum + df[[i]]
  }
  
  avg[[1]] <- sum/length(df)
  
  return(avg)
}

percent <- 6

avg.7 <- avgMat(sub.net.7.c)
avg.4 <- avgMat(sub.net.4.c)

mat_group.7.b <- formatAdjMatrix(avg.7, percent, 1)     # COMPARE 
mat_group.7.w <- formatAdjMatrix(avg.7, percent, 0)

mat_group.4.b <- formatAdjMatrix(avg.4, percent, 1)     #
mat_group.4.w <- formatAdjMatrix(avg.4, percent, 0)

mat_b.7.b   <- formatAdjMatrix(sub.net.b.7.c, percent, 1)   # COMPARE
mat_nb.7.b  <- formatAdjMatrix(sub.net.nb.7.c, percent, 1)  #
mat_al.7.b  <- formatAdjMatrix(sub.net.al.7.c, percent, 1)  # COMPARE
mat_nal.7.b <- formatAdjMatrix(sub.net.nal.7.c, percent, 1) #

mat_b.4.b   <- formatAdjMatrix(sub.net.b.4.c, percent, 1)
mat_nb.4.b  <- formatAdjMatrix(sub.net.nb.4.c, percent, 1)
mat_al.4.b  <- formatAdjMatrix(sub.net.al.4.c, percent, 1)
mat_nal.4.b <- formatAdjMatrix(sub.net.nal.4.c, percent, 1)

mat_b.7.w   <- formatAdjMatrix(sub.net.b.7.c, percent, 0)
mat_nb.7.w  <- formatAdjMatrix(sub.net.nb.7.c, percent, 0)
mat_al.7.w  <- formatAdjMatrix(sub.net.al.7.c, percent, 0)
mat_nal.7.w <- formatAdjMatrix(sub.net.nal.7.c, percent, 0)

mat_b.4.w   <- formatAdjMatrix(sub.net.b.4.c, percent, 0)
mat_nb.4.w  <- formatAdjMatrix(sub.net.nb.4.c, percent, 0)
mat_al.4.w  <- formatAdjMatrix(sub.net.al.4.c, percent, 0)
mat_nal.4.w <- formatAdjMatrix(sub.net.nal.4.c, percent, 0)


write.csv(sub.net_cor_b, "E:/Users/spectR/Desktop/%Rutgers/RESEARCH/MSN/b_matrices.csv")

write.csv(sub.net_cor_w, "E:/Users/spectR/Desktop/%Rutgers/RESEARCH/MSN/w_matrices.csv")
















#igraph stuff - outdated
graph_b <- list()
graph_w <- list()

for(i in 1:1113){
  graph_b[[i]] <- graph_from_adjacency_matrix(sub.net_cor_b[[i]], weighted=NULL, mode="undirected", diag=FALSE)
  graph_w[[i]] <- graph_from_adjacency_matrix(sub.net_cor_w[[i]], weighted=TRUE, mode="undirected", diag=FALSE)
}

plot(graph_b[[1]])


#averaging pearson correlations for group 
#sum_1 <- matrix(0L, nrow = 68, ncol = 68)
sum_2 <- matrix(0L, nrow = 68, ncol = 68)

for(i in 1:1113){
  sum_1 <- sum_1 + group_1_cor[[i]]
  sum_2 <- sum_2 + group_2_cor[[i]]
}

avg_1 <- sum_1/1113
avg_2 <- sum_2/1113



threshold_num <- .5

simMatrix_1 <- threshold(avg_1,threshold_num)
simMatrix_2 <- threshold(avg_2,threshold_num)

#converting adjacency matrices to iGraph format 
graph_1 <- graph_from_adjacency_matrix(simMatrix_1, weighted=TRUE, mode="undirected", diag=FALSE)
graph_2 <- graph_from_adjacency_matrix(simMatrix_2, weighted=TRUE, mode="undirected", diag=FALSE)

#degree - number of adjacent edges
degree_1 <- degree(graph_1)
degree_2 <- degree(graph_2)

#graph plot parameters
E(graph_2)$width <- (E(graph_2)$weight)

#circle size relative to vertex degree, rich club > 40
size_2 <- array()
for(i in 1:length(degree_2)){
  if(degree_2[i] > 40){
    size_2[i] <- (degree_2[i]-30)/2  
  } else {
    size_2[i] <- 3
  }
}
V(graph_2)$size <- size_2

#text size
V(graph_2)$label.cex = 0.8


#color based on region's pearson average
colors <- c("blue", "dark green", "red")
pearson_avg_2 <- vector()
for(i in 1:68){
  pearson_avg_2[i] <- (sum(avg_2[i,])-1)/67
}

min <- min(pearson_avg_2)
max <- max(pearson_avg_2)
for(i in 1:68){
  colors_2[i] <- as.integer((pearson_avg_2[i] - min)/(max-min) * 2.999 + 1)
}

V(graph_2)$color <- colors[match(colors_2,c(1,2,3))]

#coloring the edges
edge.start <- ends(graph_2, es = E(graph_2), names = F)[,1]
edge.col <- V(graph_2)$color[edge.start]


#plotting graphs
set.seed(7)
plot(graph_1, main = "1")

set.seed(7)
plot(graph_2, main = "2", edge.width = 1, edge.color = edge.col, vertex.label = NA)
set.seed(7)
plot(graph_2, main = "2", edge.width = 1)


#centrality scores
#principal eigenvector of t(A)*A
auth_score_1 <- authority_score(graph_1)
auth_score_2 <- authority_score(graph_2)

#principal eigenvector of A*t(A)
hub_score_1 <- hub_score(graph_1)
hub_score_2 <- hub_score(graph_2)

#eigen centrality
eigen_score_1 <- eigen_centrality(graph_1)
eigen_score_2 <- eigen_centrality(graph_2)

#Boncich power centrality
power_score_1 <- power_centrality(graph_1)
power_score_2 <- power_centrality(graph_2)

#Bonacich alpha centrality
alpha_score_1 <- alpha_centrality(graph_1)
alpha_score_2 <- alpha_centrality(graph_2)



#closeness - cant have negative values
close_score_1 <- closeness(graph_1)

#betweenness - can't have negative values
between_1 <- betweenness(graph_1)
between_2 <- betweenness(graph_2)

#distances
dist_tab_1 <- distance_table(graph_1,directed = FALSE)
dist_tab_2 <- distance_table(graph_2,directed = FALSE)

average.path.length(graph_1)
average.path.length(graph_2)

distances(graph_2, algorithm="bellman-ford")
distances(graph_2, algorithm="johnson")
distances(graph_2, algorithm="dijkstra")

all_shortest_paths(graph_2, from = 1, to = V(graph_2))

shortest_paths(graph_2, from = 1, to = 2)

#edge identification
edge_1 <- E(graph_1)
edge_2 <- E(graph_2)

vertex_1 <- V(graph_1)
vertex_2 <- V(graph_2)


