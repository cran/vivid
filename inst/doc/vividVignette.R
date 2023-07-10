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

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(1701)
#  rf <- randomForest(y ~ ., data = myData)

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(1701)
#  viviRf  <- vivi(fit = rf,
#                  data = myData,
#                  response = "y",
#                  gridSize = 50,
#                  importanceType = "agnostic",
#                  nmax = 500,
#                  reorder = TRUE,
#                  predictFun = NULL,
#                  numPerm = 4,
#                  showVimpError = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  viviHeatmap(mat = viviRf)

## ---- echo = F,  out.width = '100%'-------------------------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfheatmap.png")

## ---- eval = FALSE------------------------------------------------------------
#  viviNetwork(mat = viviRf)

## ---- echo = F,  out.width = '100%'-------------------------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfnetwork.png")

## ---- eval = FALSE------------------------------------------------------------
#  viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = FALSE)
#  viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = TRUE)

## ---- echo = F,  out.width = '100%'-------------------------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfnet_filter_comb1.png")

## ---- eval = FALSE------------------------------------------------------------
#  viviNetwork(mat = viviRf,
#              layout = cbind(c(1,1,1,1,2,2,2,2,2), c(1,2,4,5,1,2,3,4,5)))

## ---- echo = F,  out.width = '70%', fig.align='center'------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfnet_custom_layout.png")

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(1701)
#  # clustered and filtered network for rf
#  intVals <- viviRf
#  diag(intVals) <- NA
#  
#  
#  # select VIVI values in top 20%
#  impTresh <- quantile(diag(viviRf),.8)
#  intThresh <- quantile(intVals,.8,na.rm=TRUE)
#  sv <- which(diag(viviRf) > impTresh |
#                apply(intVals, 1, max, na.rm=TRUE) > intThresh)
#  
#  h <- hclust(-as.dist(viviRf[sv,sv]), method="single")
#  
#  viviNetwork(viviRf[sv,sv],
#              cluster = cutree(h, k = 3), # specify number of groups
#              layout = igraph::layout_as_star)

## ---- echo = F,  out.width = '80%', fig.align='center'------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfnet_cluster.png")

## ---- eval = FALSE------------------------------------------------------------
#  top5 <- colnames(viviRf)[1:5]
#  pdpVars(data = myData,
#          fit = rf,
#          response = 'y',
#          vars = top5,
#          nIce = 100)

## ---- echo = F,  out.width = '100%', fig.align='center'-----------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfpdp.png")

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(1701)
#  pdpPairs(data = myData,
#           fit =  rf,
#           response = "y",
#           nmax = 500,
#           gridSize = 10,
#           vars = c("x1", "x2", "x3", "x4", "x5"),
#           nIce = 100)

## ---- echo = F,  out.width = '80%', fig.align='center'------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfGPGP_subset.png")

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(1701)
#  pdpZen(data = myData, fit = rf, response = "y", nmax = 500, gridSize = 10)

## ---- echo = F,  out.width = '60%', fig.align='center'------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfzen_all.png")

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(1701)
#  pdpZen(data = myData,
#         fit = rf,
#         response = "y",
#         nmax = 500,
#         gridSize = 10,
#         zpath = c("x1", "x2", "x3", "x4", "x5"))

## ---- echo = F,  out.width = '70%', fig.align='center'------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfzen_subset.png")

## ---- eval = FALSE------------------------------------------------------------
#  # set zpaths with different parameters
#  intVals <- viviRf
#  diag(intVals) <- NA
#  intThresh <- quantile(intVals, .90, na.rm=TRUE)
#  zpSw <- zPath(viv = viviRf, cutoff = intThresh, connect = FALSE, method = 'strictly.weighted')
#  
#  
#  
#  set.seed(1701)
#  pdpZen(data = myData,
#         fit = rf,
#         response = "y",
#         nmax = 500,
#         gridSize = 10,
#         zpath = zpSw)

## ---- echo = F,  out.width = '70%', fig.align='center'------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfzen_SW.png")

## ---- eval = FALSE------------------------------------------------------------
#  library("xgboost")
#  gbst <- xgboost(data = as.matrix(myData[,1:9]),
#                  label =  as.matrix(myData[,10]),
#                  nrounds = 100,
#                  verbose = 0)

## ---- eval = FALSE------------------------------------------------------------
#  # predict function for GBM
#  pFun <- function(fit, data, ...) predict(fit, as.matrix(data[,1:9]))
#  
#  set.seed(1701)
#  viviGBst <- vivi(fit = gbst,
#                   data = myData,
#                   response = "y",
#                   reorder = FALSE,
#                   normalized = FALSE,
#                   predictFun = pFun,
#                   gridSize = 50,
#                   nmax = 500)

## ---- eval = FALSE------------------------------------------------------------
#  viviHeatmap(mat = viviGBst)

## ---- echo = F,  out.width = '100%', fig.align='center'-----------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/gbmheat.png")

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(1701)
#  rfEmbedded <- randomForest(y ~ ., data = myData, importance = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  viviRfEmbedded <- vivi(fit = rfEmbedded,
#                         data = myData,
#                         response = "y",
#                         importanceType = "%IncMSE")

## ---- eval = FALSE------------------------------------------------------------
#  rang <- ranger(y~., data = myData, importance = 'impurity')

## ---- eval = FALSE------------------------------------------------------------
#  viviRangEmbedded <- vivi(fit = rang,
#                           data = myData,
#                           response = "y",
#                           importanceType = "impurity")

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(1701)
#  rfClassif <- ranger(Species~ ., data = iris, probability = T,
#                      importance = "impurity")
#  
#  set.seed(101)
#  viviClassif  <- vivi(fit = rfClassif,
#                       data = iris,
#                       response = "Species",
#                       gridSize = 10,
#                       nmax = 50,
#                       reorder = TRUE,
#                       class = "setosa")

## ---- eval = FALSE------------------------------------------------------------
#  viviHeatmap(mat = viviClassif)
#  viviNetwork(mat = viviClassif)

## ---- echo = F,  out.width = '100%', fig.align='center'-----------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/classifVIVI.png")

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(1701)
#  pdpPairs(data = iris,
#           fit = rfClassif,
#           response = "Species",
#           class = "setosa",
#           convexHull = T,
#           gridSize = 10,
#           nmax = 50)
#  

## ---- echo = F,  out.width = '80%', fig.align='center'------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/classifGPDP.png")

