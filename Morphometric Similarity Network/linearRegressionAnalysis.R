
#7 features v 4 features
group_7_top  <- (read.csv("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/MSN/matlab/7v4/bgmat7_top.csv", header=FALSE))
group_7_rich <- read.csv("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/MSN/matlab/7v4/bgmat7_rich.csv")

group_4_top  <- (read.csv("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/MSN/matlab/7v4/bgmat4_top.csv", header=FALSE))
group_4_rich <- read.csv("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/MSN/matlab/7v4/bgmat4_rich.csv")

#Binge v No Binge
group_B_top  <- read.csv("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/MSN/matlab/BvnB/bmatb_top.csv", header=FALSE)
group_B_rich <- read.csv("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/MSN/matlab/BvnB/bmatb_rich.csv")
                        
group_nB_top  <- read.csv("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/MSN/matlab/BvnB/bmatnb_top.csv", header=FALSE)
group_nB_rich <- read.csv("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/MSN/matlab/BvnB/bmatnb_rich.csv")

#Alcohol v No Binge
group_Al_top  <- read.csv("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/MSN/matlab/AlvnAl/bmatal_top.csv", header=FALSE)
group_Al_rich <- read.csv("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/MSN/matlab/AlvnAl/bmatal_rich.csv")
                        
group_nAl_top  <- read.csv("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/MSN/matlab/AlvnAl/bmatnal_top.csv", header=FALSE)
group_nAl_rich <- read.csv("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/MSN/matlab/AlvnAl/bmatnal_rich.csv")

parseTopo <- function(df){
  degree     <- as.numeric(df[1:68])
  clust_coef <- as.numeric(df[69:136])
  char_path  <- as.numeric(df[137])
  local_eff  <- as.numeric(df[138:205])
  global_eff <- as.numeric(df[206])
  
  topo_measures <- cbind(degree,clust_coef,char_path,local_eff,global_eff)
  return(topo_measures)
}

topo_7 <- parseTopo(group_7_top)
topo_4 <- parseTopo(group_4_top)

cor(x = topo_7[,1],y = topo_4[,1])
cor(x = topo_7[,2],y = topo_4[,2])
cor(x = topo_7[,4],y = topo_4[,4])


degree74 <- lm(topo_7[,1]~topo_4[,1])
clust74 <- lm(topo_7[,2]~topo_4[,2])
loceff74 <- lm(topo_7[,4]~topo_4[,4])

topo_7[1,3]
topo_4[1,3]
topo_7[1,5]
topo_4[1,5]
summary(degree74)
summary(clust74)
summary(loceff74)

#
for(i in 1:206){
  group_B_top_avg[i] <- mean(as.numeric(group_B_top[,i]))
  group_nB_top_avg[i] <- mean(as.numeric(group_nB_top[,i]))
}

topo_b <- parseTopo(group_B_top_avg)
topo_nb <- parseTopo(group_nB_top_avg)

degreeB <- lm(topo_b[,1]~topo_nb[,1])
clustB <- lm(topo_b[,2]~topo_nb[,2])
loceffB <- lm(topo_b[,4]~topo_nb[,4])

scatter.smooth(x = topo_b[,1],y = topo_nb[,1])
scatter.smooth(x = topo_b[,2],y = topo_nb[,2])
scatter.smooth(x = topo_b[,4],y = topo_nb[,4])

topo_b[1,3]
topo_nb[1,3]
topo_b[1,5]
topo_nb[1,5]
summary(degreeB)
summary(clustB)
summary(loceffB)


#
group_Al_top_avg <- NULL
group_nAl_top_avg <- NULL

for(i in 1:206){
  group_Al_top_avg[i] <- mean(as.numeric(group_Al_top[,i]))
  group_nAl_top_avg[i] <- mean(as.numeric(group_nAl_top[,i]))
}

topo_a <- parseTopo(group_Al_top_avg)
topo_na <- parseTopo(group_nAl_top_avg)

degreeA <- lm(topo_a[,1]~topo_na[,1])
clustA <- lm(topo_a[,2]~topo_na[,2])
loceffA <- lm(topo_a[,4]~topo_na[,4])

scatter.smooth(x = topo_a[,1],y = topo_na[,1])
scatter.smooth(x = topo_a[,2],y = topo_na[,2])
scatter.smooth(x = topo_a[,4],y = topo_na[,4])

topo_a[1,3]
topo_na[1,3]
topo_a[1,5]
topo_na[1,5]
summary(degreeA)
summary(clustA)
summary(loceffA)

