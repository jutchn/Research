
#import freesurfer, make sure to use stringsAsFactors=FALSE yz 06/05/2020
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



#type 1- binge, 2- no binge, 3- alc, 4- no alc


freesurferMSN <- function(freesurfer, group, bd = FALSE, aud = FALSE, binary = TRUE, weighted = TRUE, percent = 6, featSel = 0, average = FALSE){
  subjects <- group[,1]
  
  freesurferTrim <- freesurfer[freesurfer[, 1] %in% subjects, ]
  freesurferTrim_group <- cbind(group,freesurferTrim)[,-6:-7]
  
  freesurfer_binge <- freesurferTrim_group[freesurferTrim_group[, 5] == 1, ]
  freesurfer_nobinge <- freesurferTrim_group[freesurferTrim_group[, 5] == 0, ] 
  freesurfer_alc <- freesurferTrim_group[freesurferTrim_group[, 4] == 1, ]
  freesurfer_noalc<- freesurferTrim_group[freesurferTrim_group[, 4] == 0, ]
  
  numFeat <- 7
  
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

  features1 <- parseFeatures(freesurfer_binge)  
  features2 <- parseFeatures(freesurfer_nobinge)
  features3 <- parseFeatures(freesurfer_alc)
  features4 <- parseFeatures(freesurfer_noalc)
  
  #sorting region names alphabetically and assign region names
  
  formatFeatures <- function(features){
    for(i in 1:numFeat){
      features[[i]] <- features[[i]][,sort(names(features[[i]]))]
    }
    roi.nm <- substr(colnames(features[[1]]), start = 4, stop=nchar(colnames(features[[1]]))-5)
    for(i in 1:numFeat){
      colnames(features[[i]]) <- roi.nm
    }
    return(features)
  }
  
  formattedFeatures <- formatFeatures(features)
  #extracing names
  roi.nm<-colnames(formattedFeatures[[1]])
  
  formattedFeatures1 <- formatFeatures(features1)
  formattedFeatures2 <- formatFeatures(features2)
  formattedFeatures3 <- formatFeatures(features3)
  formattedFeatures4 <- formatFeatures(features4)
  
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
  featSelect <- NULL
  if(featSel == 0){
    featSelect <- c(1:7)
  }
  while(featSel > 0){
    numEx <- featSel %% 10
    featSel <- as.integer(featSel/10)
    featSelect <- c(numEx,featSelect)
  }
  
  sub.net <- buildNet(formattedFeatures, featSelect)
  
  sub.net1 <- buildNet(formattedFeatures1, featSelect)
  sub.net2 <- buildNet(formattedFeatures2, featSelect)
  sub.net3 <- buildNet(formattedFeatures3, featSelect)
  sub.net4 <- buildNet(formattedFeatures4, featSelect)
  
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
  
  sub.net.z <- zscore(sub.net)
  
  if(bd == TRUE){
    sub.net1.z <- zscore(sub.net1)
    sub.net2.z <- zscore(sub.net2)
  }
  if(aud == TRUE){
    sub.net3.z <- zscore(sub.net3)
    sub.net4.z <- zscore(sub.net4)
  }
  
  #calculating pearson correlation for each subject
  pearson_matrix <- function(df){
    df_cor <- list()
    for(i in 1:length(df)){
      df_cor[[i]] <- cor(df[[i]], use="complete.obs", method = "pearson")
    }
    return(df_cor)
  }
  
  sub.net.c <- pearson_matrix(sub.net.z)
  
  sub.net1.c <- pearson_matrix(sub.net1.z)
  sub.net2.c <- pearson_matrix(sub.net2.z)
  sub.net3.c <- pearson_matrix(sub.net3.z)
  sub.net4.c <- pearson_matrix(sub.net4.z)
  
  #thresholding, creating binary/weighted matrices
  threshold <- function(df,thresh_num){
    if(weighted == TRUE){
      pos <- df
      neg <- df
    }
    for(i in 1:length(df[1,])){
      for(j in 1:length(df[,1])){
        if((df[i,j] < thresh_num) && (binary == TRUE)){
          df[i,j] <- df[i,j] * 0
        } else {
          if(binary == TRUE){
            df[i,j] <- 1
          }
        }
          if(weighted == TRUE) {
            if(pos[i,j] < 0){
              pos[i,j] <- pos[i,j] * 0
            }
            if(neg[i,j] > 0){
              neg[i,j] <- neg[i,j] * 0
            }
          }
        }
      }
    diag(df) <- 0
    if(weighted == TRUE){
      diag(pos) <- 0
      diag(neg) <- 0
    }
    dflist <- list()
    
    if(binary == TRUE){
      dflist[[1]] <- df
    }
    if(weighted == TRUE){
      dflist[[2]] <- pos
      dflist[[3]] <- neg
    }
    return(dflist)
  }
  
  #formatting adj matrix, applying threshold and removing diagonals
  formatAdjMatrix <- function(df, percent){
    numSubjects <- length(df)
    numRegions <- nrow(df[[1]])
    
    df_format <- df
    top_num <- as.integer(percent * .01 * numRegions * (numRegions-1))
    if(top_num %% 2==1){
      top_num <- top_num + 1
    }
    for(i in 1:numSubjects){
      thresh_num <- sort(df[[i]], TRUE)[numRegions + top_num]
      df_format[[i]] <- threshold(df[[i]],thresh_num)
    }
    df_list <- list()
    bin <- list()
    pos <- list()
    neg <- list()
    for(i in 1:numSubjects){
      if(binary == TRUE){
        bin[[i]] <- df_format[[i]][[1]]
      }
      if(weighted == TRUE){
        pos[[i]] <- df_format[[i]][[2]]
        neg[[i]] <- df_format[[i]][[3]]
      }
    }
    df_list <- list(bin,pos,neg)
    return(df_list)
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
  
    avg <- avgMat(sub.net.c)
    
    
    avg1 <- avgMat(sub.net1.c)
    avg2 <- avgMat(sub.net2.c)
    avg3 <- avgMat(sub.net3.c)
    avg4 <- avgMat(sub.net4.c)
    
    
    
    avgMat <- formatAdjMatrix(avg, percent)
    
    avgMat1 <- formatAdjMatrix(avg1, percent)
    avgMat2 <- formatAdjMatrix(avg2, percent)
    avgMat3 <- formatAdjMatrix(avg3, percent)
    avgMat4 <- formatAdjMatrix(avg4, percent)
    
    
    mat <- formatAdjMatrix(sub.net.c, percent)
    
    mat1 <- formatAdjMatrix(sub.net1.c, percent)
    mat2 <- formatAdjMatrix(sub.net2.c, percent)
    mat3 <- formatAdjMatrix(sub.net3.c, percent)
    mat4 <- formatAdjMatrix(sub.net4.c, percent)
  
  result <- list("Full Binary" = mat[[1]],
                 "Binge Binary" = mat1[[1]],
                 "no Binge Binary" = mat2[[1]],
                 "Alcohol Binary" = mat3[[1]],
                 "no Alcohol Binary" = mat4[[1]],
                 
                 "Full Weighted Positive" = mat[[2]],
                 "Binge Weighted Positive" = mat1[[2]],
                 "no Binge Weighted Positive" = mat2[[2]],
                 "Alcohol Weighted Positive" = mat3[[2]],
                 "no Alcohol Weighted Positive" = mat4[[2]],
                 
                 "Full Weighted Negative" = mat[[3]],
                 "Binge Weighted Negative" = mat1[[3]],
                 "no Binge Weighted Negative" = mat2[[3]],
                 "Alcohol Weighted Negative" = mat3[[3]],
                 "no Alcohol Weighted Negative" = mat4[[3]],
                 
                 "Avg Full Binary" = avgMat[[1]][[1]],
                 "Avg Binge Binary" = avgMat1[[1]][[1]],
                 "Avg no Binge Binary" = avgMat2[[1]][[1]],
                 "Avg Alcohol Binary" = avgMat3[[1]][[1]],
                 "Avg no Alcohol Binary" = avgMat4[[1]][[1]],
                 
                 "Avg Full Weighted Positive" = avgMat[[1]][[2]],
                 "Avg Binge Weighted Positive" = avgMat1[[1]][[2]],
                 "Avg no Binge Weighted Positive" = avgMat2[[1]][[2]],
                 "Avg Alcohol Weighted Positive" = avgMat3[[1]][[2]],
                 "Avg no Alcohol Weighted Positive" = avgMat4[[1]][[2]],
                 
                 "Avg Full Weighted Negative" = avgMat[[1]][[3]],
                 "Avg Binge Weighted Negative" = avgMat1[[1]][[3]],
                 "Avg no Binge Weighted Negative" = avgMat2[[1]][[3]],
                 "Avg Alcohol Weighted Negative" = avgMat3[[1]][[3]],
                 "Avg no Alcohol Weighted Negative" = avgMat4[[1]][[3]])
  
  if(average == FALSE){
    result[grep("Avg",names(result))] <- NULL
  }
  if(weighted == FALSE){
    result[grep("Weighted",names(result))] <- NULL
  }
  if(bd == FALSE){
    result[grep("Binge",names(result))] <- NULL
  }
  if(aud == FALSE){
    result[grep("Alcohol",names(result))] <- NULL
  }
  
  return(result)
}

freesurfer <- read.csv("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/freesurfer.csv", stringsAsFactors = FALSE)
group <- readRDS("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/MSN/hcp.dt1.rds")

test <- freesurferMSN(freesurfer,group,type = 0)

write.csv(result[[16]], "E:/Users/spectR/Desktop/%Rutgers/RESEARCH/MSN/avg_full_binary.csv")


#topological measurements
# at node level: local efficiency, node degree, clustering coefficient, and characteristic path lengths
# at global level: global efficiency, Rich club coefficient

install.packages("igraph")
install.packages("DirectedClustering")

library(igraph)
library(DirectedClustering)

#Uses adjacency matrix to calculate and return list of topological measurements: local efficiency, node degree, clustering coefficient, 
# characteristic path lengths, betweenness ,global efficiency and Rich club coefficient
topo_meas <- function(mat, binary = TRUE){
  if(binary == TRUE){
    graph <- graph_from_adjacency_matrix(mat, weighted = NULL, mode="undirected", diag = FALSE)
  } else {
    graph <- graph_from_adjacency_matrix(mat, weighted = TRUE, mode="undirected", diag = FALSE)
  }
  
  degree <- degree(graph)
  clustcoef <- ClustBCG(mat)[2]
  charpath <- average.path.length(graph)
  between <- betweenness(graph, directed = FALSE)
  
  #from brainGraph
  check_weights <- function(g, weights) {
    if (is.null(weights) && 'weight' %in% edge_attr_names(g)) {
      weights <- NULL
    } else {
      if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
      } else {
        weights <- NA
      }
    }
    return(weights)
  }
  #from brainGraph
  efficiency <- function(g, type=c('local', 'nodal', 'global'), weights=NULL,
                         use.parallel=TRUE, A=NULL) {
    stopifnot(is_igraph(g))
    i <- NULL
    weights <- check_weights(g, weights)
    
    type <- match.arg(type)
    if (type == 'local') {
      if (is.null(weights)) {
        if (is.null(A)) A <- as_adj(g, names=FALSE, attr='weight')
        A <- as.matrix(A)  #added JC 6/19/20
        weighted <- TRUE
      } else {
        A <- as_adj(g, names=FALSE, sparse=FALSE)
        weighted <- NULL
      }
      eff <- rep(0, nrow(A))
      nodes <- which(rowSums((A > 0) + 0) > 1)
      X <- apply(A, 1, function(x) which(x > 0))
      
      if (length(nodes) > 0) {
        for (i in nodes) {
          g.sub <- graph_from_adjacency_matrix(A[X[[i]], X[[i]]], mode='undirected', weighted=weighted)
          eff[i] <- efficiency(g.sub, 'global', weights=weights)
        }
      }
    } else {
      D <- distances(g, weights=weights)
      Nv <- nrow(D)
      Dinv <- 1 / D
      eff <- colSums(Dinv * is.finite(Dinv), na.rm=T) / (Nv - 1)
      charpath <- colSums(D * is.finite(D), na.rm=T) / (Nv - 1)
      if (type == 'global') eff <- sum(eff) / length(eff)
    }
    return(eff)
  }
  
  localeff <- efficiency(graph, type = 'local')
  globaleff <- efficiency(graph, type = 'global')
  
  #rich club from brainGraph
  check_degree <- function(g) {
    if ('degree' %in% vertex_attr_names(g)) {
      x <- V(g)$degree
    } else {
      x <- degree(g)
    }
    return(x)
  }
  #from brainGraph
  rich_club_coeff <- function(g, k=1, weighted=FALSE) {
    stopifnot(is_igraph(g))
    degs <- check_degree(g)
    Nv <- vcount(g)
    Nk <- sum(degs > k)
    if (Nk == 0) {
      return(list(phi=NaN, graph=make_empty_graph(), Nk=0, Ek=0))
    } else {
      rich.club.nodes <- order(degs)[(Nv - Nk + 1):Nv]
      rich.club.graph <- induced_subgraph(g, rich.club.nodes)
      Ek <- ecount(rich.club.graph)
      
      if (isTRUE(weighted)) {
        Wr <- sum(E(rich.club.graph)$weight)
        weights <- sort(E(g)$weight, decreasing=TRUE)[1:Ek]
        phi <- Wr / sum(weights)
      } else {
        phi <- graph.density(rich.club.graph)
      }
      
      return(list(phi=phi, graph=rich.club.graph, Nk=Nk, Ek=Ek))
    }
  }
  #from brainGraph
  rich_club_all <- function(g, weighted=FALSE) {
    stopifnot(is_igraph(g))
    k <- check_degree(g)
    R <- lapply(1:max(k), function(x) rich_club_coeff(g, x, weighted))
    phi <- vapply(R, with, numeric(1), phi)
    Nk <- vapply(R, with, numeric(1), Nk)
    Ek <- vapply(R, with, numeric(1), Ek)
    dt.rich <- data.frame(k=1:max(k), phi=phi, Nk=Nk, Ek=Ek)
    return(dt.rich)
  }
  
  richclub <- rich_club_all(graph)[2]
  
  topo <- list("Degree" = degree,
               "Clustering Coefficient" = clustcoef,
               "Characteristic Path" = charpath,
               "Betweenness" = between,
               "Local Efficiency" = localeff,
               "Global Efficiency" = globaleff,
               "Rich Club" = richclub)
  
}

test2 <- topo_meas(test[[16]], binary = TRUE)




as.numeric(avgfullbin[,1] - degreeb) #degree
avgfullbin[,2] - as.numeric(clustb[[1]]) #clust coef
avgfullbin[,3] 
avgfullbin[,4] - localeffb_brainGraph #loc eff
avgfullbin[1,5] - globaleffb_brainGraph #glob eff 


