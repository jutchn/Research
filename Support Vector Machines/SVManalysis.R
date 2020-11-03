freesurfer <- read.csv("~/Desktop/Research/HCPDataDepth/unrestricted_hcp_freesurfer.csv")
behavioral <- read.csv("~/Desktop/Research/HCPDataDepth/unrestricted_davidwangmo_3_5_2020_8_34_42.csv")
hcp.sub <- read.csv("~/Desktop/Research/HCPDataDepth/hcp.sub.csv")
hcpm.sub <- read.csv("~/Desktop/Research/HCPDataDepth/hcpm.sub.csv")
hcpf.sub <- read.csv("~/Desktop/Research/HCPDataDepth/hcpf.sub.csv")

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

freesurferGendered <- split(freesurfer,freesurfer$Gender)

numBrainSegments <- 68
numAttributes <- 7

namesOfSegments <- colnames(freesurfer[,grep("_Thck$",colnames(freesurfer))])
for(i in 1:numBrainSegments){
  namesOfSegments[i] <- gsub("FS_","",namesOfSegments[i])
  namesOfSegments[i] <- gsub("_Thck","",namesOfSegments[i])
}
namesOfSegments <- sort(namesOfSegments,decreasing = FALSE)

#Full
freesurfer[,grep("NumVert", colnames(freesurfer))]<- NULL
freesurfer[,grep("ThckStd", colnames(freesurfer))]<- NULL

trimmedFreesurfer <- freesurfer[, grep("_Thck$|_Area$|_GrayVol$|_FoldInd$|_CurvInd$|_GausCurv$|_MeanCurv$" ,colnames(freesurfer))]
trimmedFreesurfer <- trimmedFreesurfer[,order(colnames(trimmedFreesurfer))]

indices <- seq(1, by = numAttributes, length = ncol(trimmedFreesurfer) / numAttributes) #Get Indices to split trimmedFreeSurfer
splitBrain <- lapply(indices, function(i, trimmedFreesurfer) trimmedFreesurfer[i:(i+numAttributes-1)], trimmedFreesurfer = trimmedFreesurfer)

MBDSplitBrain <- lapply(splitBrain, MBD)

fullMBDValues <- vector()
for (i in 1:numBrainSegments){
  fullMBDValues <- rbind(fullMBDValues, MBDSplitBrain[[i]]$MBD)
}
fullMBDValues <- t(fullMBDValues)
colnames(fullMBDValues) <- namesOfSegments

#Males
freesurferGendered$M[,grep("NumVert", colnames(freesurferGendered$M))]<- NULL
freesurferGendered$M[,grep("ThckStd", colnames(freesurferGendered$M))]<- NULL

trimmedFreesurferMale <- freesurferGendered$M[, grep("_Thck$|_Area$|_GrayVol$|_FoldInd$|_CurvInd$|_GausCurv$|_MeanCurv$" ,colnames(freesurferGendered$M))]
trimmedFreesurferMale <- trimmedFreesurferMale[,order(colnames(trimmedFreesurferMale))]

splitBrainMale <- lapply(indices, function(i, trimmedFreesurferMale) trimmedFreesurferMale[i:(i+numAttributes-1)], trimmedFreesurferMale = trimmedFreesurferMale)

MBDSplitBrainMale <- lapply(splitBrainMale, MBD)

fullMBDValuesMale <- vector()
for (i in 1:numBrainSegments){
  fullMBDValuesMale <- rbind(fullMBDValuesMale, MBDSplitBrainMale[[i]]$MBD)
}
fullMBDValuesMale <- t(fullMBDValuesMale)
colnames(fullMBDValuesMale) <- namesOfSegments

#Females
freesurferGendered$F[,grep("NumVert", colnames(freesurferGendered$F))]<- NULL
freesurferGendered$F[,grep("ThckStd", colnames(freesurferGendered$F))]<- NULL

trimmedFreesurferFemale <- freesurferGendered$F[, grep("_Thck$|_Area$|_GrayVol$|_FoldInd$|_CurvInd$|_GausCurv$|_MeanCurv$" ,colnames(freesurferGendered$F))]
trimmedFreesurferFemale <- trimmedFreesurferFemale[,order(colnames(trimmedFreesurferFemale))]

splitBrainFemale <- lapply(indices, function(i, trimmedFreesurferFemale) trimmedFreesurferFemale[i:(i+numAttributes-1)], trimmedFreesurferFemale = trimmedFreesurferFemale)

MBDSplitBrainFemale <- lapply(splitBrainFemale, MBD)

fullMBDValuesFemale <- vector()
for (i in 1:numBrainSegments){
  fullMBDValuesFemale <- rbind(fullMBDValuesFemale, MBDSplitBrainFemale[[i]]$MBD)
}
fullMBDValuesFemale <- t(fullMBDValuesFemale)
colnames(fullMBDValuesFemale) <- namesOfSegments

#SVM
library(e1071)

fullMBDValues <- cbind(freesurfer$Subject, fullMBDValues)
colnames(fullMBDValues)[1] <- "Subject"
fullMBDValuesMale <- cbind(freesurfer$Subject[which(freesurfer$Gender == "M")], fullMBDValuesMale)
colnames(fullMBDValuesMale)[1] <- "Subject"
fullMBDValuesFemale <- cbind(freesurfer$Subject[which(freesurfer$Gender == "F")], fullMBDValuesFemale)
colnames(fullMBDValuesFemale)[1] <- "Subject"

mergeHCP <- merge(hcp.sub, fullMBDValues, by = "Subject")
mergeHCPm <- merge(hcpm.sub, fullMBDValuesMale, by = "Subject")
mergeHCPf <- merge(hcpf.sub, fullMBDValuesFemale, by = "Subject")

mergeHCP[c(1:2)] <- NULL

HCPDataType <- split(mergeHCP, mergeHCP$Type)
train_set <- HCPDataType$Train
test_set <- HCPDataType$Test

#Train Set
train_set[c(2)] <- NULL
splitRegions <- lapply(2:69, function(i, train_set) cbind(train_set$Gender,train_set[i],rep(i-1,420)), train_set = train_set)
for(i in 1:68) {
  names(splitRegions[[i]]) <- c("Gender", "MBD", "Region")
}
modifiedTrain_set <- vector()
for (i in 1:numBrainSegments){
  modifiedTrain_set <- rbind(modifiedTrain_set, splitRegions[[i]])
}

train_set <- cbind(c(1:420), train_set)
names(train_set)[1] <- "indices"

#Test Set
test_set[c(2)] <- NULL
splitRegions <- lapply(2:69, function(i, test_set) cbind(test_set$Gender,test_set[i],rep(i-1,515)), test_set = test_set)
for(i in 1:68) {
  names(splitRegions[[i]]) <- c("Gender", "MBD", "Region")
}
modifiedTest_set <- vector()
for (i in 1:numBrainSegments){
  modifiedTest_set <- rbind(modifiedTest_set, splitRegions[[i]])
}

test_set <- cbind(c(1:515), test_set)
names(test_set)[1] <- "indices"

#Plotting trained
classifier = svm(formula = Gender ~ ., data = modifiedTrain_set, type = 'C-classification', kernel = 'linear') 
plot(classifier, modifiedTrain_set)

classifier2 = svm(formula = Gender ~ ., data = train_set, type = 'C-classification', kernel = 'linear') 
plot(classifier2, train_set, R_Lingual~indices)

#Testing
prediction <- predict(classifier, newdata = modifiedTest_set)
cm = table(modifiedTest_set[,3], prediction)
cm #confusion matrix

prediction <- predict(classifier2, newdata = test_set)
cm2 = table(test_set[,1], prediction)
cm2 <- as.data.frame(cm2)
cm2 <- cm2[-c(516:1030),]
for(i in 1:515) {
  if(cm2$Freq[i] == 0) {
    cm2$prediction[i] = 'M'
  }
}
names(cm2)[1] <- "indices"
test_set <- merge(cm2, test_set, by = "indices")
count <- 0
for(i in 1:515) {
  if(test_set$prediction[i] == test_set$Gender[i])
    count <- count+1
}

Emotion <- behavioral[, c(168:191)]
Cognitive <- behavioral[, c(116:167)]
Personality <- behavioral[, c(508:572)]
Alertness <- behavioral[, c(88:115)]
Motor <- behavioral[, c(501:507)]

Emotion <- cbind(behavioral$Subject, Emotion)
colnames(Emotion)[1] <- "Subject"
Cognitive <- cbind(behavioral$Subject, Cognitive)
colnames(Cognitive)[1] <- "Subject"
Personality <- cbind(behavioral$Subject, Personality)
colnames(Personality)[1] <- "Subject"
Alertness <- cbind(behavioral$Subject, Alertness)
colnames(Alertness)[1] <- "Subject"
Motor <- cbind(behavioral$Subject, Motor)
colnames(Motor)[1] <- "Subject"

Emotion <- na.omit(Emotion)
Cognitive <- na.omit(Cognitive)
Personality <- na.omit(Personality)
Alertness <- na.omit(Alertness)
Motor <- na.omit(Motor)

mergeHCP <- cbind(hcp.sub$Subject, mergeHCP)
colnames(mergeHCP)[1] <- "Subject"

mergeHCPemotion <- merge(mergeHCP, Emotion, by = "Subject")
mergeHCPcognitive <- merge(mergeHCP, Cognitive, by = "Subject")
mergeHCPpersonality <- merge(mergeHCP, Personality, by = "Subject")
mergeHCPalertness <- merge(mergeHCP, Alertness, by = "Subject")
mergeHCPmotor <- merge(mergeHCP, Motor, by = "Subject")

mergeHCPemotionM <- merge(mergeHCPm, Emotion, by = "Subject")
mergeHCPcognitiveM <- merge(mergeHCPm, Cognitive, by = "Subject")
mergeHCPpersonalityM <- merge(mergeHCPm, Personality, by = "Subject")
mergeHCPalertnessM <- merge(mergeHCPm, Alertness, by = "Subject")
mergeHCPmotorM <- merge(mergeHCPm, Motor, by = "Subject")

mergeHCPemotionF <- merge(mergeHCPf, Emotion, by = "Subject")
mergeHCPcognitiveF <- merge(mergeHCPf, Cognitive, by = "Subject")
mergeHCPpersonalityF <- merge(mergeHCPf, Personality, by = "Subject")
mergeHCPalertnessF <- merge(mergeHCPf, Alertness, by = "Subject")
mergeHCPmotorF <- merge(mergeHCPf, Motor, by = "Subject")

emotionDataType <- split(mergeHCPemotion,mergeHCPemotion$Type)
emotionTrain_set <- emotionDataType$Train

cognitiveDataType <- split(mergeHCPcognitive,mergeHCPcognitive$Type)
cognitiveTrain_set <- cognitiveDataType$Train

personalityDataType <- split(mergeHCPpersonality,mergeHCPpersonality$Type)
personalityTrain_set <- personalityDataType$Train

alertnessDataType <- split(mergeHCPalertness,mergeHCPalertness$Type)
alertnessTrain_set <- alertnessDataType$Train

motorDataType <- split(mergeHCPmotor,mergeHCPmotor$Type)
motorTrain_set <- motorDataType$Train


regressionSVM <- function(xValue, yValue, dataset) {
    xAxis = substitute(xValue)
    yAxis = substitute(yValue)
    plot(xValue, yValue, xlab = xAxis, ylab = yAxis)
    model <- svm(yValue ~ xValue, dataset)
    predictedY <- predict(model, dataset)
    points(xValue, predictedY, col = "red", pch=4)
}
regressionSVM(cognitiveTrain_set$L_Bankssts, cognitiveTrain_set$DDisc_AUC_40K, cognitiveTrain_set)


