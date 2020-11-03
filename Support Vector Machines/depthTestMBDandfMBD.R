freesurfer <- read.csv("~/Desktop/Research/HCPDataDepth/unrestricted_hcp_freesurfer.csv")
behavioral <- read.csv("~/Desktop/Research/HCPDataDepth/unrestricted_davidwangmo_3_5_2020_8_34_42.csv")
hcp.sub <- read.csv("~/Desktop/Research/HCPDataDepth/hcp.sub.csv")
hcpm.sub <- read.csv("~/Desktop/Research/HCPDataDepth/hcpm.sub.csv")
hcpf.sub <- read.csv("~/Desktop/Research/HCPDataDepth/hcpf.sub.csv")

#combinat
combinat = function(n,p){
  if (n<p){combinat=0}
  else {combinat=exp(lfactorial(n)-(lfactorial(p)+lfactorial(n-p)))}
}

#fMBD
fMBD = function(data){
  p=dim(data)[1]
  n=dim(data)[2]
  rmat=apply(data,1,rank)
  down=rmat-1
  up=n-rmat
  (rowSums(up*down)/p+n-1)/combinat(n,2)
}
#MBD
MBD = function(x, xRef=NULL)
{
  n <- nrow(x); d <- ncol(x) # n: number of observations (samples);  d: dimension of the data
  x <- as.matrix(x)
  
  if (length(xRef)==0) {  ## MBD with respect to the same sample
    
    ## depth computation
    if (ncol(x) == 1) {x <- t(x)}
    depth <- matrix(0,1,n)
    ordered.matrix <- x
    if (n>1) {
      for (columns in 1:d) {
        ordered.matrix[,columns] <- sort(x[,columns])
        for (element in 1:n) {
          index.1 <- length(which(ordered.matrix[,columns] < x[element,columns]))
          index.2 <- length(which(ordered.matrix[,columns] <= x[element,columns]))
          multiplicity <- index.2 - index.1
          depth[element] <- depth[element] + index.1 * (n - (index.2)) + multiplicity * (n - index.2 + index.1) + choose(multiplicity,2)
        }   ### end FOR element
      }  ### end FOR columns
      depth <- depth / (d * choose(n,2) )
    } ## end IF
    if (n==1) {deepest <- x; depth <- 0}
    ordering<-order(depth,decreasing=TRUE)
    
  } ## end IF no reference sample
  
  else {
    xRef <- as.matrix(xRef)
    if (ncol(xRef)!=d) {stop("Dimensions of x and xRef do not match")}
    n0 <- nrow(xRef)
    
    ## depth computations
    if (ncol(x) == 1) {x <- t(x)}
    depth <- matrix(0,1,n)
    ordered.matrix <- xRef
    if (n0>1) {
      for (columns in 1:d) {
        ordered.matrix[,columns] <- sort(xRef[,columns])
        for (element in 1:n) {
          index.1 <- length(which(ordered.matrix[,columns] < x[element,columns]))
          index.2 <- length(which(ordered.matrix[,columns] <= x[element,columns]))
          multiplicity <- index.2 - index.1
          depth[element] <- depth[element] + (index.1 + multiplicity ) * (n0 - index.1 - multiplicity) + multiplicity * ( index.1 + (multiplicity-1)/2)
        }   ### end FOR element
      }   ### end FOR columns
      depth <- depth / (d * choose(n0,2) )
    } ## end IF
    if (n==1) {deepest <- x; depth <- 0}
    ordering<-order(depth,decreasing=TRUE)
    
  }  ## end ELSE
  return(list(ordering=ordering,MBD=depth))
}

library(depthTests)

freesurferGendered <- split(freesurfer,freesurfer$Gender)

thicknessM <- freesurferGendered$M[, grep("_Thck$" , colnames(freesurfer))]
areaM <- freesurferGendered$M[, grep("_Area", colnames(freesurfer))]
grayVolM <- freesurferGendered$M[, grep("_GrayVol", colnames(freesurfer))]
foldingIndexM <- freesurferGendered$M[, grep("_FoldInd" , colnames(freesurfer))]
curveIndexM <- freesurferGendered$M[, grep("_CurvInd" , colnames(freesurfer))]
gausCurvM <- freesurferGendered$M[, grep("_GausCurv" , colnames(freesurfer))]
meanCurvM <- freesurferGendered$M[, grep("_MeanCurv" , colnames(freesurfer))]

thicknessF <- freesurferGendered$F[, grep("_Thck$" , colnames(freesurfer))]
areaF <- freesurferGendered$F[, grep("_Area", colnames(freesurfer))]
grayVolF <- freesurferGendered$F[, grep("_GrayVol", colnames(freesurfer))]
foldingIndexF <- freesurferGendered$F[, grep("_FoldInd" , colnames(freesurfer))]
curveIndexF <- freesurferGendered$F[, grep("_CurvInd" , colnames(freesurfer))]
gausCurvF <- freesurferGendered$F[, grep("_GausCurv" , colnames(freesurfer))]
meanCurvF <- freesurferGendered$F[, grep("_MeanCurv" , colnames(freesurfer))]

thickness <- freesurfer[, grep("_Thck$" , colnames(freesurfer))]
area <- freesurfer[, grep("_Area", colnames(freesurfer))]
grayVol <- freesurfer[, grep("_GrayVol", colnames(freesurfer))]
foldingIndex <- freesurfer[, grep("_FoldInd" , colnames(freesurfer))]
curveIndex <- freesurfer[, grep("_CurvInd" , colnames(freesurfer))]
gausCurv <- freesurfer[, grep("_GausCurv" , colnames(freesurfer))]
meanCurv <- freesurfer[, grep("_MeanCurv" , colnames(freesurfer))]

thicknessM <- thicknessM[ , order(names(thicknessM))]
thicknessF <- thicknessF[ , order(names(thicknessF))]
thickness <- thickness[ , order(names(thickness))]

areaM <- areaM[ , order(names(areaM))]
areaF <- areaF[ , order(names(areaF))]
area <- area[ , order(names(area))]

grayVolM <- grayVolM[ , order(names(grayVolM))]
grayVolF <- grayVolF[ , order(names(grayVolF))]
grayVol <- grayVol[ , order(names(grayVol))]

foldingIndexM <- foldingIndexM[ , order(names(foldingIndexM))]
foldingIndexF <- foldingIndexF[ , order(names(foldingIndexF))]
foldingIndex <- foldingIndex[ , order(names(foldingIndex))]

curveIndexM <- curveIndexM[ , order(names(curveIndexM))]
curveIndexF <- curveIndexF[ , order(names(curveIndexF))]
curveIndex <- curveIndex[ , order(names(curveIndex))]

gausCurvM <- gausCurvM[ , order(names(gausCurvM))]
gausCurvF <- gausCurvF[ , order(names(gausCurvF))]
gausCurv <- gausCurv[ , order(names(gausCurv))]

meanCurvM <- meanCurvM[ , order(names(meanCurvM))]
meanCurvF <- meanCurvF[ , order(names(meanCurvF))]
meanCurv <- meanCurv[ , order(names(meanCurv))]

#MBD Version 1
MBD_thicknessM_ordering <- MBD(thicknessM)
MBD_thicknessF_ordering <- MBD(thicknessF)
MBD_thickness_ordering <- MBD(thickness)

MBD_areaM_ordering <- MBD(areaM)
MBD_areaF_ordering <- MBD(areaF)
MBD_area_ordering <- MBD(area)

MBD_grayVolM_ordering <- MBD(grayVolM)
MBD_grayVolF_ordering <- MBD(grayVolF)
MBD_grayVol_ordering <- MBD(grayVol)

MBD_foldingIndexM_ordering <- MBD(foldingIndexM)
MBD_foldingIndexF_ordering <- MBD(foldingIndexF)
MBD_foldingIndex_ordering <- MBD(foldingIndex)

MBD_curveIndexM_ordering <- MBD(curveIndexM)
MBD_curveIndexF_ordering <- MBD(curveIndexF)
MBD_curveIndex_ordering <- MBD(curveIndex)

MBD_gausCurvM_ordering <- MBD(gausCurvM)
MBD_gausCurvF_ordering <- MBD(gausCurvF)
MBD_gausCurv_ordering <- MBD(gausCurv)

MBD_meanCurvM_ordering <- MBD(meanCurvM)
MBD_meanCurvF_ordering <- MBD(meanCurvF)
MBD_meanCurv_ordering <- MBD(meanCurv)

male.order <-cbind(MBD_thicknessM_ordering$ordering,MBD_grayVolM_ordering$ordering,MBD_areaM_ordering$ordering,
                   MBD_foldingIndexM_ordering$ordering,MBD_curveIndexM_ordering$ordering,MBD_gausCurvM_ordering$ordering,
                   MBD_meanCurvM_ordering$ordering)
male.MBD <-rbind(MBD_thicknessM_ordering$MBD,MBD_grayVolM_ordering$MBD,MBD_areaM_ordering$MBD,MBD_foldingIndexM_ordering$MBD,
                 MBD_curveIndexM_ordering$MBD,MBD_gausCurvM_ordering$MBD,MBD_meanCurvM_ordering$MBD)
male.MBD <- t(male.MBD)

female.order <-cbind(MBD_thicknessF_ordering$ordering,MBD_grayVolF_ordering$ordering,MBD_areaF_ordering$ordering,
                     MBD_foldingIndexF_ordering$ordering,MBD_curveIndexF_ordering$ordering,MBD_gausCurvF_ordering$ordering,
                     MBD_meanCurvF_ordering$ordering)
female.MBD <-rbind(MBD_thicknessF_ordering$MBD,MBD_grayVolF_ordering$MBD,MBD_areaF_ordering$MBD,MBD_foldingIndexF_ordering$MBD,
                   MBD_curveIndexF_ordering$MBD,MBD_gausCurvF_ordering$MBD,MBD_meanCurvF_ordering$MBD)
female.MBD <- t(female.MBD)

full.order <-cbind(MBD_thickness_ordering$ordering,MBD_grayVol_ordering$ordering,MBD_area_ordering$ordering,
                   MBD_foldingIndex_ordering$ordering,MBD_curveIndex_ordering$ordering,MBD_gausCurv_ordering$ordering,
                   MBD_meanCurv_ordering$ordering)
full.MBD <-rbind(MBD_thickness_ordering$MBD,MBD_grayVol_ordering$MBD,MBD_area_ordering$MBD,MBD_foldingIndex_ordering$MBD,
                 MBD_curveIndex_ordering$MBD,MBD_gausCurv_ordering$MBD,MBD_meanCurv_ordering$MBD)
full.MBD <- t(full.MBD)

#Column name
colnames(full.MBD) <- c("thickness", "grayVol", "area", "foldingIndex", "curveIndex", "gausCurv", "meanCurv")
colnames(full.order) <- c("thickness", "grayVol", "area", "foldingIndex", "curveIndex", "gausCurv", "meanCurv")

colnames(male.MBD) <- c("thickness", "grayVol", "area", "foldingIndex", "curveIndex", "gausCurv", "meanCurv")
colnames(male.order) <- c("thickness", "grayVol", "area", "foldingIndex", "curveIndex", "gausCurv", "meanCurv")

colnames(female.MBD) <- c("thickness", "grayVol", "area", "foldingIndex", "curveIndex", "gausCurv", "meanCurv")
colnames(female.order) <- c("thickness", "grayVol", "area", "foldingIndex", "curveIndex", "gausCurv", "meanCurv")

#fMBD
fMBD_thicknessM_ordering <- fMBD(thicknessM)
fMBD_thicknessF_ordering <- fMBD(thicknessF)
fMBD_thickness_ordering <- fMBD(thickness)

fMBD_areaM_ordering <- fMBD(areaM)
fMBD_areaF_ordering <- fMBD(areaF)
fMBD_area_ordering <- fMBD(area)

fMBD_grayVolM_ordering <- fMBD(grayVolM)
fMBD_grayVolF_ordering <- fMBD(grayVolF)
fMBD_grayVol_ordering <- fMBD(grayVol)

fMBD_foldingIndexM_ordering <- fMBD(foldingIndexM)
fMBD_foldingIndexF_ordering <- fMBD(foldingIndexF)
fMBD_foldingIndex_ordering <- fMBD(foldingIndex)

fMBD_curveIndexM_ordering <- fMBD(curveIndexM)
fMBD_curveIndexF_ordering <- fMBD(curveIndexF)
fMBD_curveIndex_ordering <- fMBD(curveIndex)

fMBD_gausCurvM_ordering <- fMBD(gausCurvM)
fMBD_gausCurvF_ordering <- fMBD(gausCurvF)
fMBD_gausCurv_ordering <- fMBD(gausCurv)

fMBD_meanCurvM_ordering <- fMBD(meanCurvM)
fMBD_meanCurvF_ordering <- fMBD(meanCurvF)
fMBD_meanCurv_ordering <- fMBD(meanCurv)

male.fMBD <-cbind(fMBD_thicknessM_ordering,fMBD_grayVolM_ordering,fMBD_areaM_ordering,fMBD_foldingIndexM_ordering,
                  fMBD_curveIndexM_ordering,fMBD_gausCurvM_ordering,fMBD_meanCurvM_ordering)

female.fMBD <-cbind(fMBD_thicknessF_ordering,fMBD_grayVolF_ordering,fMBD_areaF_ordering,fMBD_foldingIndexF_ordering,
                    fMBD_curveIndexF_ordering,fMBD_gausCurvF_ordering,fMBD_meanCurvF_ordering)

full.fMBD <-cbind(fMBD_thickness_ordering,fMBD_grayVol_ordering,fMBD_area_ordering,fMBD_foldingIndex_ordering,
                  fMBD_curveIndex_ordering,fMBD_gausCurv_ordering,fMBD_meanCurv_ordering)

boxplot(female.MBD)
boxplot(male.MBD)

full.MBD <- cbind(freesurfer$Subject, full.MBD)
colnames(full.MBD)[1] <- "Subject"
male.MBD <- cbind(freesurfer$Subject[which(freesurfer$Gender == "M")], male.MBD)
colnames(male.MBD)[1] <- "Subject"
female.MBD <- cbind(freesurfer$Subject[which(freesurfer$Gender == "F")], female.MBD)
colnames(female.MBD)[1] <- "Subject"

# Pearson Correlation for Age and MBD Values
mergeHCP <- merge(full.MBD, hcp.sub, by = "Subject")
mergeHCPm <- merge(male.MBD, hcpm.sub, by = "Subject")
mergeHCPf <- merge(female.MBD, hcpf.sub, by = "Subject")

ageMBDCor <- cor(mergeHCP$Age_in_Yrs,mergeHCP[,c(2:8)])
ageMBDCorM <- cor(mergeHCPm$Age_in_Yrs,mergeHCPm[,c(2:8)])
ageMBDCorF <- cor(mergeHCPf$Age_in_Yrs,mergeHCPf[,c(2:8)])

#Pearson Correlation for Adj, Unadj and MBD Values
adjBehavioral <- behavioral[, grep("Adj$" , colnames(behavioral))]
unadjBehavioral <- behavioral[, grep("Unadj$" , colnames(behavioral))]
adjBehavioralM <- behavioral[behavioral$Gender == "M", grep("Adj$" , colnames(behavioral))]
unadjBehavioralM <- behavioral[behavioral$Gender == "M", grep("Unadj$" , colnames(behavioral))]
adjBehavioralF <- behavioral[behavioral$Gender == "F", grep("Adj$" , colnames(behavioral))]
unadjBehavioralF <- behavioral[behavioral$Gender == "F", grep("Unadj$" , colnames(behavioral))]

adjBehavioral <- cbind(behavioral$Subject, adjBehavioral)
colnames(adjBehavioral)[1] <- "Subject"
unadjBehavioral <- cbind(behavioral$Subject, unadjBehavioral)
colnames(unadjBehavioral)[1] <- "Subject"
adjBehavioralM <- cbind(behavioral$Subject[which(behavioral$Gender == "M")], adjBehavioralM)
colnames(adjBehavioralM)[1] <- "Subject"
unadjBehavioralM <- cbind(behavioral$Subject[which(behavioral$Gender == "M")], unadjBehavioralM)
colnames(unadjBehavioralM)[1] <- "Subject"
adjBehavioralF <- cbind(behavioral$Subject[which(behavioral$Gender == "F")], adjBehavioralF)
colnames(adjBehavioralF)[1] <- "Subject"
unadjBehavioralF <- cbind(behavioral$Subject[which(behavioral$Gender == "F")], unadjBehavioralF)
colnames(unadjBehavioralF)[1] <- "Subject"

mergeAdj <- merge(full.MBD,adjBehavioral, by = "Subject")
mergeUnadj <- merge(full.MBD,unadjBehavioral, by = "Subject")
mergeAdjM <- merge(male.MBD,adjBehavioralM, by = "Subject")
mergeUnadjM <- merge(male.MBD,unadjBehavioralM, by = "Subject")
mergeAdjF <- merge(female.MBD,adjBehavioralF, by = "Subject")
mergeUnadjF <- merge(female.MBD,unadjBehavioralF, by = "Subject")

mergeAdj <- na.omit(mergeAdj)
mergeUnadj <- na.omit(mergeUnadj)
mergeAdjM <- na.omit(mergeAdjM)
mergeUnadjM <- na.omit(mergeUnadjM)
mergeAdjF <- na.omit(mergeAdjF)
mergeUnadjF <- na.omit(mergeUnadjF)

adjCor <- cor(mergeAdj[,c(2:8)],mergeAdj[,c(9:24)])
unadjCor <- cor(mergeUnadj[,c(2:8)],mergeUnadj[,c(9:41)])
adjCorM <- cor(mergeAdjM[,c(2:8)],mergeAdjM[,c(9:24)])
unadjCorM <- cor(mergeUnadjM[,c(2:8)],mergeUnadjM[,c(9:41)])
adjCorF <- cor(mergeAdjF[,c(2:8)],mergeAdjF[,c(9:24)])
unadjCorF <- cor(mergeUnadjF[,c(2:8)],mergeUnadjF[,c(9:41)])

#Pearson Correlation for Impulsivity and MBD Values
impulsivity <- behavioral[, grep("DDisc_AUC", colnames(behavioral))]
impulsivityM <- behavioral[behavioral$Gender == "M", grep("DDisc_AUC" , colnames(behavioral))]
impulsivityF <- behavioral[behavioral$Gender == "F", grep("DDisc_AUC" , colnames(behavioral))]

impulsivity <- cbind(behavioral$Subject, impulsivity)
colnames(impulsivity)[1] <- "Subject"
impulsivityM <- cbind(behavioral$Subject[which(behavioral$Gender == "M")], impulsivityM)
colnames(impulsivityM)[1] <- "Subject"
impulsivityF <- cbind(behavioral$Subject[which(behavioral$Gender == "F")], impulsivityF)
colnames(impulsivityF)[1] <- "Subject"

mergeImpulsivity <- merge(full.MBD,impulsivity, by = "Subject")
mergeImpulsivityM <- merge(male.MBD,impulsivity, by = "Subject")
mergeImpulsivityF <- merge(female.MBD,impulsivity, by = "Subject")

mergeImpulsivity <- na.omit(mergeImpulsivity)
mergeImpulsivityM <- na.omit(mergeImpulsivityM)
mergeImpulsivityF <- na.omit(mergeImpulsivityF)

impulsivityCor <- cor(mergeImpulsivity[,c(2:8)],mergeImpulsivity[,c(9:10)])
impulsivityCorM <- cor(mergeImpulsivityM[,c(2:8)],mergeImpulsivityM[,c(9:10)])
impulsivityCorF <- cor(mergeImpulsivityF[,c(2:8)],mergeImpulsivityF[,c(9:10)])




