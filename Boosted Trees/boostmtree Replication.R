install.packages("boostmtree")
library(boostmtree)

dtaA100 <- simLong(n = 100, N = 5, rho =.80, q = 0, model = 2,family = "Continuous")$dtaL
dtaB100 <- simLong(n = 100, N = 15, rho =.80, q = 0, model = 2,family = "Continuous")$dtaL
dtaC100 <- simLong(n = 100, N = 5, rho =.80, q = 30, model = 2,family = "Continuous")$dtaL
dtaD100 <- simLong(n = 100, N = 5, rho =.80, q = 0, model = 2, type = "corAR1", family = "Continuous")$dtaL

dtaA500 <- simLong(n = 500, N = 5, rho =.80, q = 0, model = 2,family = "Continuous")$dtaL
dtaB500 <- simLong(n = 500, N = 15, rho =.80, q = 0, model = 2,family = "Continuous")$dtaL
dtaC500 <- simLong(n = 500, N = 5, rho =.80, q = 30, model = 2,family = "Continuous")$dtaL
dtaD500 <- simLong(n = 500, N = 5, rho =.80, q = 0, model = 2, type = "corAR1", family = "Continuous")$dtaL

boost.growA100 <- boostmtree(dtaA100$features, dtaA100$time, dtaA100$id, dtaA100$y,family = "Continuous",
                             M = 500, k = 5, nu = 0.05, verbose = TRUE, cv.flag = TRUE)
boost.growB100 <- boostmtree(dtaB100$features, dtaB100$time, dtaB100$id, dtaB100$y,family = "Continuous",
                             M = 500, k = 5, nu = 0.05, verbose = TRUE, cv.flag = TRUE)
boost.growC100 <- boostmtree(dtaC100$features, dtaC100$time, dtaC100$id, dtaC100$y,family = "Continuous",
                             M = 500, k = 5, nu = 0.05, verbose = TRUE, cv.flag = TRUE)
boost.growD100 <- boostmtree(dtaD100$features, dtaD100$time, dtaD100$id, dtaD100$y,family = "Continuous",
                             M = 500, k = 5, nu = 0.05, verbose = TRUE, cv.flag = TRUE)

boost.growA500 <- boostmtree(dtaA500$features, dtaA500$time, dtaA500$id, dtaA500$y,family = "Continuous",
                             M = 500, k = 5, nu = 0.05, verbose = TRUE, cv.flag = TRUE)
boost.growB500 <- boostmtree(dtaB500$features, dtaB500$time, dtaB500$id, dtaB500$y,family = "Continuous",
                             M = 500, k = 5, nu = 0.05, verbose = TRUE, cv.flag = TRUE)
boost.growC500 <- boostmtree(dtaC500$features, dtaC500$time, dtaC500$id, dtaC500$y,family = "Continuous",
                             M = 500, k = 5, nu = 0.05, verbose = TRUE, cv.flag = TRUE)
boost.growD500 <- boostmtree(dtaD500$features, dtaD500$time, dtaD500$id, dtaD500$y,family = "Continuous",
                             M = 500, k = 5, nu = 0.05, verbose = TRUE, cv.flag = TRUE)

sRMSE <- data.frame(boost.growA100$rmse, boost.growB100$rmse, boost.growC100$rmse, boost.growD100$rmse)
sRMSE <- sRMSE/data.frame(boost.growA100$ysd, boost.growB100$ysd, boost.growC100$ysd, boost.growD100$ysd)

plot(boost.growA100)
plot(boost.growA500)

plot(boost.growB100)
plot(boost.growB500)

plot(boost.growC100)
plot(boost.growC500)

plot(boost.growD100)
plot(boost.growD500)

sRMSE <- data.frame(boost.growA500$rmse, boost.growB500$rmse, boost.growC500$rmse, boost.growD500$rmse)

vimp.growA100 <- vimp.boostmtree(object = boost.growA100)
vimp.growB100 <- vimp.boostmtree(object = boost.growB100)
vimp.growC100 <- vimp.boostmtree(object = boost.growC100)
vimp.growD100 <- vimp.boostmtree(object = boost.growD100)

vimp.growA500 <- vimp.boostmtree(object = boost.growA500)
vimp.growB500 <- vimp.boostmtree(object = boost.growB500)
vimp.growC500 <- vimp.boostmtree(object = boost.growC500)
vimp.growD500 <- vimp.boostmtree(object = boost.growD500)

vimpPlot(vimp = vimp.growA100, Time_Interaction = TRUE, Width_Bar = 1, ymaxlim = 20, ymaxtimelim = 20,
         xaxishead = c(3,3), yaxishead = c(65,65),
         cex.xlab = 1, subhead.cexval = 1.2)
vimpPlot(vimp = vimp.growB100, Time_Interaction = TRUE, Width_Bar = 1, ymaxlim = 20, ymaxtimelim = 20,
         xaxishead = c(3,3), yaxishead = c(65,65),
         cex.xlab = 1, subhead.cexval = 1.2)
vimpPlot(vimp = vimp.growC100, Time_Interaction = TRUE, Width_Bar = .5, ymaxlim = 20, ymaxtimelim = 20,
         xaxishead = c(3,3), yaxishead = c(65,65),
         cex.xlab = 1, subhead.cexval = 1.2)
vimpPlot(vimp = vimp.growD100, Time_Interaction = TRUE, Width_Bar = 1, ymaxlim = 20, ymaxtimelim = 20,
         xaxishead = c(3,3), yaxishead = c(65,65),
         cex.xlab = 1, subhead.cexval = 1.2)
