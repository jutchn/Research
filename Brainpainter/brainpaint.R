
#read example file
template <- read.csv("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/brainpaint/template/DK_template.csv")
brainpaint <- read.csv("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/brainpaint/template/LoadExample4BrainPainter.csv")
brainpaint <- t(brainpaint)

#remove subcortical measurements
#brainpaint[2, grep('_vol',(brainpaint[1,]))] <- 0

numZeroed <- 0

#zero out ~0 values
lowerZero <- -0.03
upperZero <- 0.03

for(i in 1:220){
  if(as.numeric(brainpaint[[2,i]]) > lowerZero && as.numeric(brainpaint[2,i]) < upperZero){
    brainpaint[2,i] <- as.numeric(brainpaint[[2,i]]) * 0
    numZeroed <- numZeroed + 1
  }
}

#negtopos
#for(i in 1:220){
#  if(as.numeric(brainpaint[[2,i]]) < 0){
#    brainpaint[2,i] <- as.numeric(brainpaint[[2,i]]) * -1  
#  }
#}

#0to4
#for(i in 1:220){
#  if(as.numeric(brainpaint[[2,i]]) == 0){
#    brainpaint[2,i] <- 4
#  }
#}

#min/max
min <- as.numeric(brainpaint[[2,which.min(brainpaint[2,])]])
max <- as.numeric(brainpaint[[2,which.max(brainpaint[2,])]])
scale <- 2

bound <- min
if(abs(min) < abs(max)){
  bound <- max
}

#scale values between -3 and 3
if(min<0){
  for(i in 1:220){
    if(as.numeric(brainpaint[[2,i]]) < 0){
      brainpaint[2,i] <- (as.numeric(brainpaint[[2,i]]) * scale/abs(bound))
    } else {
      brainpaint[2,i] <- (as.numeric(brainpaint[[2,i]]) * scale/abs(bound))
    }
  }
} else {
#scale values between 0 and 3
  for(i in 1:220){
    brainpaint[2,i] <- (as.numeric(brainpaint[[2,i]]) - min)/(max-min) * scale
  }
}

#changing 0 values to white color
for(i in 1:220){
  if(as.numeric(brainpaint[[2,i]]) == 0){
    brainpaint[2,i] <- scale+1
  }
}


#split data into 3 regions + subcortical for each brain hemisphere
r_area_brainpaint <- brainpaint[, grep('fs_r_.*_area',(brainpaint[1,]))]
r_grayvol_brainpaint <- brainpaint[, grep('fs_r_.*_grayvol',(brainpaint[1,]))]
r_thck_brainpaint <- brainpaint[, grep('fs_r_.*_thck',(brainpaint[1,]))]

r_area_brainpaint[1,] <- substring(r_area_brainpaint[1,],6,(nchar(r_area_brainpaint[1,]))-5)
r_grayvol_brainpaint[1,] <- substring(r_grayvol_brainpaint[1,],6,(nchar(r_grayvol_brainpaint[1,]))-8)
r_thck_brainpaint[1,] <- substring(r_thck_brainpaint[1,],6,(nchar(r_thck_brainpaint[1,]))-5)

r_subcortical_brainpaint <- brainpaint[, grep('fs_r_.*_vol',(brainpaint[1,]))]
r_subcortical_brainpaint[1, ] <- substring(r_subcortical_brainpaint[1,],6,(nchar(r_subcortical_brainpaint[1,]))-4)


l_area_brainpaint <- brainpaint[, grep('fs_l_.*_area',(brainpaint[1,]))]
l_grayvol_brainpaint <- brainpaint[, grep('fs_l_.*_grayvol',(brainpaint[1,]))]
l_thck_brainpaint <- brainpaint[, grep('fs_l_.*_thck',(brainpaint[1,]))]

l_area_brainpaint[1,] <- substring(l_area_brainpaint[1,],6,(nchar(l_area_brainpaint[1,]))-5)
l_grayvol_brainpaint[1,] <- substring(l_grayvol_brainpaint[1,],6,(nchar(l_grayvol_brainpaint[1,]))-8)
l_thck_brainpaint[1,] <- substring(l_thck_brainpaint[1,],6,(nchar(l_thck_brainpaint[1,]))-5)

l_subcortical_brainpaint <- brainpaint[, grep('fs_l_.*_vol',(brainpaint[1,]))]
l_subcortical_brainpaint[1, ] <- substring(l_subcortical_brainpaint[1,],6,(nchar(l_subcortical_brainpaint[1,]))-4)


#function to format to match the template
format<- function(df,imgName,left){
  if(left == 1){
    df <- cbind(df,l_subcortical_brainpaint)
  } else {
    df <- cbind(df,r_subcortical_brainpaint)
  }
  df <- cbind(image.name.unique = imgName,df)
  df[1,1]<-"Image-name-unique"
  
  colnames(df) <- df[1,]
  df<- df[-1,]
}

#left boolean for which subcortical regions to append
left <- 1
#names of the generated brainpaints
l_area_brainpaint <- format(l_area_brainpaint,"left_area",left)
l_grayvol_brainpaint <- format(l_grayvol_brainpaint,"left_grayvol",left)
l_thck_brainpaint <- format(l_thck_brainpaint,"left_thck",left)

left <- 0

r_area_brainpaint <- format(r_area_brainpaint,"right_area",left)
r_grayvol_brainpaint <- format(r_grayvol_brainpaint,"right_grayvol",left)
r_thck_brainpaint <- format(r_thck_brainpaint,"right_thck",left)

#build final template, stacking
exampleBrainPaint <- rbind(l_area_brainpaint,l_grayvol_brainpaint,l_thck_brainpaint,r_area_brainpaint,r_grayvol_brainpaint,r_thck_brainpaint)
exampleBrainPaint <- as.data.frame(exampleBrainPaint)


#export
write.csv(exampleBrainPaint,"E:/Users/spectR/Desktop/%Rutgers/RESEARCH/brainpaint/template/formattedExampleBrainPaintTest.csv",row.names=FALSE)

yellow <- rgb(1, 1, 0, maxColorValue = 1)
orange <- rgb(1, 0.4, 0, maxColorValue = 1)
red <- rgb(1, 0, 0, maxColorValue = 1)

cyan <- rgb(0, 1, 1, maxColorValue = 1)
azure <- rgb(0, 0.5, 1, maxColorValue = 1)
blue <- rgb(0, 0, 1, maxColorValue = 1)


colfunc_warm <- colorRampPalette(c(yellow, orange, red))
colfunc_cold <- colorRampPalette(c(blue, azure, cyan))


z=matrix(1:100,nrow=1)
x=1
y=seq(0,abs(bound),len=100)
image(x,y,z,col=colfunc_warm(100),axes=FALSE,xlab="",ylab="")
axis(2)


x=seq(abs(bound)*-1,0,len=100)
image(x,y,z,col=colfunc_cold(100),axes=FALSE,xlab="",ylab="")
axis(2)



