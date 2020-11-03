install.packages("igraph")
library(igraph)

#import freesurfer
freesurfer <- read.csv("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/freesurfer.csv")

#parse 7 features
thickness <- freesurfer[, grep("_Thck$" , colnames(freesurfer))]
area <- freesurfer[, grep("_Area", colnames(freesurfer))]
grayVol <- freesurfer[, grep("_GrayVol", colnames(freesurfer))]
foldingIndex <- freesurfer[, grep("_FoldInd" , colnames(freesurfer))]
curveIndex <- freesurfer[, grep("_CurvInd" , colnames(freesurfer))]
gausCurv <- freesurfer[, grep("_GausCurv" , colnames(freesurfer))]
meanCurv <- freesurfer[, grep("_MeanCurv" , colnames(freesurfer))]

#sorting region names alphabetically 
thickness <- thickness[,sort(names(thickness))]
area <- area[,sort(names(area))]
grayVol <- grayVol[,sort(names(grayVol))]

foldingIndex <- foldingIndex[,sort(names(foldingIndex))]
curveIndex <- curveIndex[,sort(names(curveIndex))]
gausCurv <- gausCurv[,sort(names(gausCurv))]
meanCurv <- meanCurv[,sort(names(meanCurv))]

#collecting region names
crnames <- c(colnames(thickness))
col_row_names <- list()
for(i in 1:length(crnames)){
  col_row_names[i] <- substr(crnames[i],4,nchar(crnames[i])-5)
}

#renaming columns for rbind
colnames(thickness) <- col_row_names
colnames(area) <- col_row_names
colnames(grayVol) <- col_row_names

colnames(foldingIndex) <- col_row_names
colnames(curveIndex) <- col_row_names
colnames(gausCurv) <- col_row_names
colnames(meanCurv) <- col_row_names

#z-scoring features
thickness_numeric <- as.numeric(unlist(thickness))
thickness_z <- (thickness - mean(thickness_numeric))/sd(thickness_numeric)

area_numeric <- as.numeric(unlist(area))
area_z <- (area - mean(area_numeric))/sd(area_numeric)

grayVol_numeric <- as.numeric(unlist(grayVol))
grayVol_z <- (grayVol - mean(grayVol_numeric))/sd(grayVol_numeric)

foldingIndex_numeric <- as.numeric(unlist(foldingIndex))
foldingIndex_z <- (foldingIndex - mean(foldingIndex_numeric))/sd(foldingIndex_numeric)

curveIndex_numeric <- as.numeric(unlist(curveIndex))
curveIndex_z <- (curveIndex - mean(curveIndex_numeric))/sd(curveIndex_numeric)

gausCurv_numeric <- as.numeric(unlist(gausCurv))
gausCurv_z <- (gausCurv - mean(gausCurv_numeric))/sd(gausCurv_numeric)

meanCurv_numeric <- as.numeric(unlist(meanCurv))
meanCurv_z <- (meanCurv - mean(meanCurv_numeric))/sd(meanCurv_numeric)

#making 2 groups - 3 features and 7 features
group_1 <- list()
group_2 <- list()

for(i in 1:1113){
  group_1[[i]] <- rbind(thickness_z[i,],area_z[i,],grayVol_z[i,])
  group_2[[i]] <- rbind(thickness_z[i,],area_z[i,],grayVol_z[i,],foldingIndex_z[i,],curveIndex_z[i,],gausCurv_z[i,],meanCurv_z[i,])
}

#calculating pearson correlation of 3x68 or 7x68 matrices for each subject
group_1_cor <- list()
group_2_cor <- list()

for(i in 1:1113){
  group_1_cor[[i]] <- cor(group_1[[i]], use="complete.obs", method = "pearson")
  group_2_cor[[i]] <- cor(group_2[[i]], use="complete.obs", method = "pearson")
}

#averaging pearson correlations for group 
sum_1 <- matrix(0L, nrow = 68, ncol = 68)
sum_2 <- matrix(0L, nrow = 68, ncol = 68)

for(i in 1:1113){
  sum_1 <- sum_1 + group_1_cor[[i]]
  sum_2 <- sum_2 + group_2_cor[[i]]
}

avg_1 <- sum_1/1113
avg_2 <- sum_2/1113

#thresholding values
threshold <- function(df,x){
  for(i in 1:length(df[1,])){
    for(j in 1:length(df[,1])){
      if(df[i,j] < x && df[i,j] > x *-1){
        df[i,j] <- df[i,j] * 0
      } else{
        df[i,j] <- df[i,j]
      }
    }
  }
  return(df)
}
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


