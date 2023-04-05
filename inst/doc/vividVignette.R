## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "vig/"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup, echo = FALSE, warning=FALSE, message=FALSE------------------------
library(vivid)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(vivid) # for visualisations 
library(randomForest) # for model fit
library(ranger)       # for model fit

## ---- messages = FALSE--------------------------------------------------------
set.seed(101)
genFriedman <- function(noFeatures = 10,
                        noSamples = 100,
                        sigma = 1
) {
  # Set Values
  n <- noSamples # no of rows
  p <- noFeatures # no of variables
  e <- rnorm(n, sd = sigma)
  
  
  # Create matrix of values
  xValues <- matrix(runif(n * p, 0, 1), nrow = n) # Create matrix
  colnames(xValues) <- paste0("x", 1:p) # Name columns
  df <- data.frame(xValues) # Create dataframe
  
  
  # Equation:
  # y = 10sin(πx1x2) + 20(x3−0.5)^2 + 10x4 + 5x5 + ε
  y <- (10 * sin(pi * df$x1 * df$x2) + 20 * (df$x3 - 0.5)^2 + 10 * df$x4 + 5 *    df$x5 + e)
  # Adding y to df
  df$y <- y
  df
}

myData <- genFriedman(noFeatures = 9, noSamples = 350, sigma = 1)

## -----------------------------------------------------------------------------
set.seed(1701)
rf <- randomForest(y ~ ., data = myData, importance = TRUE)

## -----------------------------------------------------------------------------
set.seed(1701)
viviRf  <- vivi(fit = rf, 
                data = myData, 
                response = "y",
                gridSize = 50,
                importanceType = "%IncMSE",
                nmax = 500,
                reorder = TRUE,
                class = 1,
                predictFun = NULL,
                numPerm = 4)

## -----------------------------------------------------------------------------
viviHeatmap(mat = viviRf)

## -----------------------------------------------------------------------------
viviNetwork(mat = viviRf)

## -----------------------------------------------------------------------------
viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = FALSE)
viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = TRUE)

## -----------------------------------------------------------------------------
viviNetwork(mat = viviRf, 
            layout = cbind(c(1,1,1,1,2,2,2,2,2), c(1,2,4,5,1,2,3,4,5)))

## -----------------------------------------------------------------------------
set.seed(1701)
viviNetwork(mat = viviRf, cluster = igraph::cluster_fast_greedy)

## -----------------------------------------------------------------------------
top5 <- colnames(viviRf)[1:5]
pdpVars(data = myData,
        fit = rf,
        response = 'y',
        vars = top5)

## -----------------------------------------------------------------------------
set.seed(1701)
pdpPairs(data = myData, 
         fit =  rf, 
         response = "y", 
         nmax = 500, 
         gridSize = 10,         
         vars = c("x1", "x2", "x3", "x4", "x5"),
         nIce = 100)

## -----------------------------------------------------------------------------
set.seed(1701)
pdpZen(data = myData, fit = rf, response = "y", nmax = 500, gridSize = 10)

## -----------------------------------------------------------------------------
set.seed(1701)
pdpZen(data = myData, 
       fit = rf, 
       response = "y",
       nmax = 500, 
       gridSize = 10, 
       zpath = c("x1", "x2", "x3", "x4", "x5"))

## -----------------------------------------------------------------------------
# set zpaths with different parameters
intVals <- viviRf
diag(intVals) <- NA
intThresh <- quantile(intVals, .90, na.rm=TRUE)
zpSw <- zPath(viv = viviRf, cutoff = intThresh, connect = FALSE, method = 'strictly.weighted')



set.seed(1701)
pdpZen(data = myData, 
       fit = rf, 
       response = "y",
       nmax = 500, 
       gridSize = 10, 
       zpath = zpSw)

## -----------------------------------------------------------------------------
library("xgboost")
gbst <- xgboost(data = as.matrix(myData[,1:9]),
                label =  as.matrix(myData[,10]),
                nrounds = 100,
                verbose = 0)

## -----------------------------------------------------------------------------
# predict function for GBM
pFun <- function(fit, data, ...) predict(fit, as.matrix(data[,1:9]))

set.seed(1701)
viviGBst <- vivi(fit = gbst,
                 data = myData,
                 response = "y",
                 reorder = FALSE,
                 normalized = FALSE,
                 predictFun = pFun,
                 gridSize = 50,
                 nmax = 500)

## -----------------------------------------------------------------------------
viviHeatmap(mat = viviGBst)

## -----------------------------------------------------------------------------
set.seed(1701)
rfClassif <- ranger(Species~ ., data = iris, probability = T, 
                    importance = "impurity")

set.seed(101)
viviClassif  <- vivi(fit = rfClassif, 
                     data = iris, 
                     response = "Species",
                     gridSize = 10,
                     nmax = 50,
                     reorder = TRUE,
                     class = "setosa")

## -----------------------------------------------------------------------------
viviHeatmap(mat = viviClassif)
viviNetwork(mat = viviClassif)

## -----------------------------------------------------------------------------
set.seed(1701)
pdpPairs(data = iris, 
         fit = rfClassif, 
         response = "Species",
         class = "setosa",  
         convexHull = T, 
         gridSize = 10, 
         nmax = 50) 

