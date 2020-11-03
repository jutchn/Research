library(boostmtree)

## Not run:
##------------------------------------------------------------
## Synthetic example (Response is continuous)
## High correlation, quadratic time with quadratic interaction
##-------------------------------------------------------------
#simulate the data
dta <- simLong(n = 10, N = 10, rho =.80, model = 2,family = "Continuous")$dtaL
#basic boosting call
boost.grow <- boostmtree(dta$features, dta$time, dta$id, dta$y, family = "Continuous", M = 300, cv.flag = TRUE)
#plot results
#x1 has a linear main effect
#x2 is quadratic with quadratic time trend
marginalPlot(boost.grow, "x1",plot.it = TRUE)
marginalPlot(boost.grow, "x2",plot.it = TRUE)
marginalPlot(boost.grow, "x3",plot.it = TRUE)
marginalPlot(boost.grow, "x4",plot.it = TRUE)
plot(boost.grow)

vimp.grow <- vimp.boostmtree(object = boost.grow)

vimpPlot(vimp = vimp.grow, ymaxlim = 20, ymaxtimelim = 20,
         xaxishead = c(3,3), yaxishead = c(65,65),
         cex.xlab = 1, subhead.cexval = 1.2)


## Not run:
##------------------------------------------------------------
## Synthetic example (Response is continuous)
## VIMP is based on in-sample CV using out of bag data
##-------------------------------------------------------------

#simulate the data
dtaVIMP1 <- simLong(n = 50, N = 5, rho =.80, model = 2,family = "Continuous")$dtaL

#basic boosting call
boost.grow <- boostmtree(dtaVIMP1$features, dtaVIMP1$time, dtaVIMP1$id, dtaVIMP1$y,
                         family = "Continuous", M = 300,cv.flag = TRUE)
vimp.grow <- vimp.boostmtree(object = boost.grow,x.names=c("x1","x2"),joint = FALSE)
vimp.joint.grow <- vimp.boostmtree(object = boost.grow,x.names=c("x1","x2"),joint = TRUE)

vimp.grow2 <- vimp.boostmtree(object = boost.grow)

vimpPlot(vimp = vimp.grow2, ymaxlim = 20, ymaxtimelim = 20,
         xaxishead = c(3,3), yaxishead = c(65,65),
         cex.xlab = 1, subhead.cexval = 1.2)

##------------------------------------------------------------
## Synthetic example (Response is continuous)
## VIMP is based on test data
##-------------------------------------------------------------

#simulate the data
dtaO <- simLong(n = 100, ntest = 100, N = 5, rho =.80, model = 2, family = "Continuous")

## save the data as both a list and data frame
dtaL <- dtaO$dtaL
dta <- dtaO$dta

## get the training data
trn <- dtaO$trn

#basic boosting call
boost.grow2 <- boostmtree(dtaL$features[trn,], dtaL$time[trn], dtaL$id[trn], dtaL$y[trn],
                          family = "Continuous", M = 300)
boost.pred <- predict(boost.grow,dtaL$features[-trn,], dtaL$time[-trn], dtaL$id[-trn],
                      dtaL$y[-trn])
vimp.pred <- vimp.boostmtree(object = boost.pred,x.names=c("x1","x2"),joint = FALSE)
vimp.joint.pred <- vimp.boostmtree(object = boost.pred,x.names=c("x1","x2"),joint = TRUE)

vimp.pred2 <- vimp.boostmtree(object = boost.grow)

vimpPlot(vimp = vimp.pred2, ymaxlim = 20, ymaxtimelim = 20,
         xaxishead = c(3,3), yaxishead = c(65,65),
         cex.xlab = 1, subhead.cexval = 1.2)
## End(Not run)


##----------------------------------------------------------------------------
## spirometry data
##----------------------------------------------------------------------------
data(spirometry, package = "boostmtree")

#boosting call: cubic B-splines with 15 knots
spr.obj <- boostmtree(spirometry$features, spirometry$time, spirometry$id, spirometry$y,
                      family = "Continuous",M = 300, nu = .025, nknots = 15)

#marginal plot of double-lung group at 5 years
dltx <- marginalPlot(spr.obj, "AGE", tm.unq = 5, subset = spr.obj$x$DOUBLE==1,plot.it = TRUE)

#marginal plot of single-lung group at 5 years
sltx <- marginalPlot(spr.obj, "AGE", tm.unq = 5, subset = spr.obj$x$DOUBLE==0,plot.it = TRUE)

#combine the two plots
dltx <- dltx[[2]][[1]]
sltx <- sltx[[2]][[1]]
plot(range(c(dltx[[1]][, 1], sltx[[1]][, 1])), range(c(dltx[[1]][, -1], sltx[[1]][, -1])),
     xlab = "age", ylab = "predicted y", type = "n")
lines(dltx[[1]][, 1][order(dltx[[1]][, 1]) ], dltx[[1]][, -1][order(dltx[[1]][, 1]) ],
      lty = 1, lwd = 2, col = "red")
lines(sltx[[1]][, 1][order(sltx[[1]][, 1]) ], sltx[[1]][, -1][order(sltx[[1]][, 1]) ],
      lty = 1, lwd = 2, col = "blue")
legend("topright", legend = c("DLTx", "SLTx"), lty = 1, fill = c(2,4))

## End(Not run)

marginalPlot(spr.obj,plot.it = TRUE)



## Not run:
##------------------------------------------------------------
## Synthetic example (Response is continuous)
##
## High correlation, quadratic time with quadratic interaction
## largish number of noisy variables
##
## Illustrates how modified gradient improves performance
## also compares performance to ideal and well specified linear models
##----------------------------------------------------------------------------

## simulate the data
## simulation 2: main effects (x1, x3, x4), quad-time-interaction (x2)
dtaO <- simLong(n = 100, ntest = 100, model = 2, family = "Continuous", q = 25)

## save the data as both a list and data frame
dtaL <- dtaO$dtaL
dta <- dtaO$dta

## get the training data
trn <- dtaO$trn

## save formulas for linear model comparisons
f.true <- dtaO$f.true
f.linr <- "y~g( x1+x2+x3+x4+x1*time+x2*time+x3*time+x4*time )"

## modified tree gradient (default)
o.1 <- boostmtree(dtaL$features[trn, ], dtaL$time[trn], dtaL$id[trn],dtaL$y[trn],
                  family = "Continuous",M = 350)
p.1 <- predict(o.1, dtaL$features[-trn, ], dtaL$time[-trn], dtaL$id[-trn], dtaL$y[-trn])

## non-modified tree gradient (nmtg)
o.2 <- boostmtree(dtaL$features[trn, ], dtaL$time[trn], dtaL$id[trn], dtaL$y[trn],
                  family = "Continuous",M = 350, mod.grad = FALSE)
p.2 <- predict(o.2, dtaL$features[-trn, ], dtaL$time[-trn], dtaL$id[-trn], dtaL$y[-trn])

## set rho = 0
o.3 <- boostmtree(dtaL$features[trn, ], dtaL$time[trn], dtaL$id[trn], dtaL$y[trn],
                  family = "Continuous",M = 350, rho = 0)
p.3 <- predict(o.3, dtaL$features[-trn, ], dtaL$time[-trn], dtaL$id[-trn], dtaL$y[-trn])

##rmse values compared to generalized least squares (GLS)
##for true model and well specified linear models (LM)
cat("true LM :", boostmtree:::gls.rmse(f.true,dta,trn),"\n")
cat("well specified LM :", boostmtree:::gls.rmse(f.linr,dta,trn),"\n")
cat("boostmtree :", p.1$rmse,"\n")
cat("boostmtree (nmtg):", p.2$rmse,"\n")
cat("boostmtree (rho=0):", p.3$rmse,"\n")

##predicted value plots
plot(p.1)
plot(p.2)
plot(p.3)



