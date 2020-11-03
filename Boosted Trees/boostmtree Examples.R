install.packages("boostmtree")
install.packages("mlbench")
library(boostmtree)
library(mlbench)
##------------------------------------------------------------
## synthetic example (Response y is continuous)
## 0.8 correlation, quadratic time with quadratic interaction
##-------------------------------------------------------------
#simulate the data (use a small sample size for illustration)
dta <- simLong(n = 50, N = 5, rho =.80, model = 2,family = "Continuous")$dtaL
plot(1:4,dta$features[1,])
#basic boosting call (M set to a small value for illustration)
boost.grow <- boostmtree(dta$features, dta$time, dta$id, dta$y,family = "Continuous",M = 20, verbose = FALSE)

#print results
print(boost.grow)

#plot.results
plot(boost.grow)

##------------------------------------------------------------
## synthetic example (Response y is binary)
## 0.8 correlation, quadratic time with quadratic interaction
##-------------------------------------------------------------
#simulate the data (use a small sample size for illustration)
dta <- simLong(n = 50, N = 5, rho =.80, model = 2, family = "Binary")$dtaL

#basic boosting call (M set to a small value for illustration)
boost.grow <- boostmtree(dta$features, dta$time, dta$id, dta$y,family = "Binary", M = 20)

#print results
print(boost.grow)

#plot.results
plot(boost.grow)

## Not run: 
##------------------------------------------------------------
## Same synthetic example as above with continuous response
## but with in-sample cross-validation estimate for RMSE
##-------------------------------------------------------------
dta <- simLong(n = 50, N = 5, rho =.80, model = 2,family = "Continuous")$dtaL
boost.cv.grow <- boostmtree(dta$features, dta$time, dta$id, dta$y,
                            family = "Continuous", M = 300, cv.flag = TRUE)
plot(boost.cv.grow)
print(boost.cv.grow)

##----------------------------------------------------------------------------
## spirometry data (Response is continuous)
##----------------------------------------------------------------------------
data(spirometry, package = "boostmtree")

#boosting call: cubic B-splines with 15 knots
spr.obj <- boostmtree(spirometry$features, spirometry$time, spirometry$id, spirometry$y,
                      family = "Continuous",M = 100, nu = .025, nknots = 15)
plot(spr.obj)


##----------------------------------------------------------------------------
## Atrial Fibrillation data (Response is binary)
##----------------------------------------------------------------------------
data(AF, package = "boostmtree")

#boosting call: cubic B-splines with 15 knots
AF.obj <- boostmtree(AF$feature, AF$time, AF$id, AF$y,
                     family = "Binary",M = 100, nu = .025, nknots = 15)
plot(AF.obj)


##----------------------------------------------------------------------------
## sneaky way to use boostmtree for (univariate) regression: boston housing
##----------------------------------------------------------------------------

if (library("mlbench", logical.return = TRUE)) {
  
  ## assemble the data
  data(BostonHousing)
  x <- BostonHousing; x$medv <- NULL
  y <- BostonHousing$medv
  trn <- sample(1:nrow(x), size = nrow(x) * (2 / 3), replace = FALSE)
  
  ## run boosting in univariate mode
  o <- boostmtree(x = x[trn,], y = y[trn],family = "Continuous")
  o.p <- predict(o, x = x[-trn, ], y = y[-trn])
  print(o)
  plot(o.p)
  
  ## run boosting in univariate mode to obtain RMSE and vimp
  o.cv <- boostmtree(x = x, y = y, M = 100,family = "Continuous",cv.flag = TRUE)
  print(o.cv)
  plot(o.cv)
}


## End(Not run)