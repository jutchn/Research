install.packages("randomForestSRC", repos = "http://cran.us.r-project.org")
install.packages("boostmtree", repos = "http://cran.us.r-project.org")

library(boostmtree)
library(parallel)

numIterations <- 100
learningRate <- 0.05
numLeaf <- 5

#Male

sim.m <- readRDS("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/simulated/simu.m.rds")
sim.m <- readRDS("/home/jc2549/research/boostmtree/sim/simu.m.rds")
sim.m<-sim.m[1:100,]

sim.m.predictors <-sim.m[,1:220]
sim.m.predictors <- as.data.frame(sim.m.predictors)

sim.m.age <- sim.m[,221]
sim.m.rb <- sim.m[,222]
sim.m.negurg <- sim.m[,223]
sim.m.sesk <- sim.m[,224]

trn <- sample(1:nrow(sim.m.predictors), size = nrow(sim.m.predictors) * (2 / 3), replace = FALSE)


#AGE
boost.grow.m.age <- boostmtree(x = sim.m.predictors[trn, ], y = sim.m.age[trn],family = "Continuous",
                             M = numIterations, k = numLeaf, nu = learningRate, cv.flag = TRUE)
boost.predict.m.age <- predict(boost.grow.m.age, x = sim.m.predictors[-trn, ], y = sim.m.age[-trn])
#plot(boost.grow.m.age)
#plot(boost.predict.m.age)
vimp.grow.m.age <- vimp.boostmtree(boost.grow.m.age, joint = FALSE)
vimp.predict.m.age <- vimp.boostmtree(boost.predict.m.age, joint = FALSE)
#vimpPlot(vimp.predict.m.age)


#RB
boost.grow.m.rb <- boostmtree(x = sim.m.predictors[trn, ], y = sim.m.rb[trn],family = "Continuous",
                               M = numIterations, k = numLeaf, nu = learningRate, cv.flag = TRUE)
boost.predict.m.rb <- predict(boost.grow.m.rb, x = sim.m.predictors[-trn, ], y = sim.m.rb[-trn])
#plot(boost.grow.m.rb)
#plot(boost.predict.m.rb)
vimp.grow.m.rb <- vimp.boostmtree(boost.grow.m.rb, joint = FALSE)
vimp.predict.m.rb <- vimp.boostmtree(boost.predict.m.rb, joint = FALSE)
#vimpPlot(vimp.predict.m.rb)


#NEGURG
boost.grow.m.negurg <- boostmtree(x = sim.m.predictors[trn, ], y = sim.m.negurg[trn],family = "Continuous",
                               M = numIterations, k = numLeaf, nu = learningRate, cv.flag = TRUE)
boost.predict.m.negurg <- predict(boost.grow.m.negurg, x = sim.m.predictors[-trn, ], y = sim.m.negurg[-trn])
#plot(boost.grow.m.negurg)
#plot(boost.predict.m.negurg)
vimp.grow.m.negurg <- vimp.boostmtree(boost.grow.m.negurg, joint = FALSE)
vimp.predict.m.negurg <- vimp.boostmtree(boost.predict.m.negurg, joint = FALSE)
#vimpPlot(vimp.predict.m.negurg)


#SESK
boost.grow.m.sesk <- boostmtree(x = sim.m.predictors[trn, ], y = sim.m.sesk[trn],family = "Continuous",
                               M = numIterations, k = numLeaf, nu = learningRate, cv.flag = TRUE)
boost.predict.m.sesk <- predict(boost.grow.m.sesk, x = sim.m.predictors[-trn, ], y = sim.m.sesk[-trn])

#boost.predict.m.sesk <- predict(boost.grow.m.sesk, x = sim.m.predictors[-trn, ])

plot(boost.grow.m.sesk)
plot(boost.predict.m.sesk)

vimp.grow.m.sesk <- vimp.boostmtree(boost.grow.m.sesk, joint = FALSE)
vimp.predict.m.sesk <- vimp.boostmtree(boost.predict.m.sesk, joint = FALSE)
vimpPlot(vimp.predict.m.sesk)



#Female

#sim.f <- readRDS("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/simulated/simu.f.rds")
sim.f <- readRDS("/home/jc2549/research/boostmtree/sim/simu.f.rds")


sim.f.predictors <-sim.f[,1:220]
sim.f.predictors <- as.data.frame(sim.f.predictors)

sim.f.age <- sim.f[,221]
sim.f.rb <- sim.f[,222]
sim.f.negurg <- sim.f[,223]
sim.f.sesk <- sim.f[,224]

trn <- sample(1:nrow(sim.f.predictors), size = nrow(sim.f.predictors) * (2 / 3), replace = FALSE)


#AGE
boost.grow.f.age <- boostmtree(x = sim.f.predictors[trn, ], y = sim.f.age[trn],family = "Continuous",
                               M = numIterations, k = numLeaf, nu = learningRate, cv.flag = TRUE)
boost.predict.f.age <- predict(boost.grow.f.age, x = sim.f.predictors[-trn, ], y = sim.f.age[-trn])
#plot(boost.grow.f.age)
#plot(boost.predict.f.age)
vimp.grow.f.age <- vimp.boostmtree(boost.grow.f.age, joint = FALSE)
vimp.predict.f.age <- vimp.boostmtree(boost.predict.f.age, joint = FALSE)
#vimpPlot(vimp.predict.f.age)


#RB
boost.grow.f.rb <- boostmtree(x = sim.f.predictors[trn, ], y = sim.f.rb[trn],family = "Continuous",
                              M = numIterations, k = numLeaf, nu = learningRate, cv.flag = TRUE)
boost.predict.f.rb <- predict(boost.grow.f.rb, x = sim.f.predictors[-trn, ], y = sim.f.rb[-trn])
#plot(boost.grow.f.rb)
#plot(boost.predict.f.rb)
vimp.grow.f.rb <- vimp.boostmtree(boost.grow.f.rb, joint = FALSE)
vimp.predict.f.rb <- vimp.boostmtree(boost.predict.f.rb, joint = FALSE)
#vimpPlot(vimp.predict.f.rb)


#NEGURG
boost.grow.f.negurg <- boostmtree(x = sim.f.predictors[trn, ], y = sim.f.negurg[trn],family = "Continuous",
                                  M = numIterations, k = numLeaf, nu = learningRate, cv.flag = TRUE)
boost.predict.f.negurg <- predict(boost.grow.f.negurg, x = sim.f.predictors[-trn, ], y = sim.f.negurg[-trn])
#plot(boost.grow.f.negurg)
#plot(boost.predict.f.negurg)
vimp.grow.f.negurg <- vimp.boostmtree(boost.grow.f.negurg, joint = FALSE)
vimp.predict.f.negurg <- vimp.boostmtree(boost.predict.f.negurg, joint = FALSE)
#vimpPlot(vimp.predict.f.negurg)


#SESK
boost.grow.f.sesk <- boostmtree(x = sim.f.predictors[trn, ], y = sim.f.sesk[trn],family = "Continuous",
                                M = numIterations, k = numLeaf, nu = learningRate, cv.flag = TRUE)
boost.predict.f.sesk <- predict(boost.grow.f.sesk, x = sim.f.predictors[-trn, ], y = sim.f.sesk[-trn])
#plot(boost.grow.f.sesk)
#plot(boost.predict.f.sesk)
vimp.grow.f.sesk <- vimp.boostmtree(boost.grow.f.sesk, joint = FALSE)
vimp.predict.f.sesk <- vimp.boostmtree(boost.predict.f.sesk, joint = FALSE)
#vimpPlot(vimp.predict.f.sesk)


#save.image("E:/Users/spectR/Desktop/%Rutgers/RESEARCH/simulated/testexport.RData")
save.image("/home/jc2549/research/boostmtree/sim/dataExport.RData")