## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup, echo = FALSE, warning=FALSE, message=FALSE------------------------
library(vivid)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(vivid) # for visualisations 
library(randomForest) # for model fit
library(mlr3)         # for model fit
library(mlr3learners) # for model fit
library(ranger)       # for model fit
library(ggplot2) 

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
set.seed(100)
rf <- randomForest(y ~ ., data = myData, importance = TRUE)

## ---- message = F, warning = F------------------------------------------------
set.seed(101)
rf_fit  <- vivi(fit = rf, 
                data = myData, 
                response = "y",
                gridSize = 10,
                importanceType = "%IncMSE",
                nmax = 100,
                reorder = TRUE,
                class = 1,
                predictFun = NULL)

## ---- fig.width=6, fig.height=6, fig.align='center'---------------------------
viviHeatmap(mat = rf_fit) + ggtitle("rf heatmap")

## ---- fig.width=6, fig.height=6, fig.align='center'---------------------------
viviNetwork(mat = rf_fit)

## ---- fig.width=6, fig.height=6, fig.align='center'---------------------------
viviNetwork(mat = rf_fit, intThreshold = 0.12, removeNode = F)

## ---- fig.width=6, fig.height=6, fig.align='center'---------------------------
viviNetwork(mat = rf_fit, intThreshold = 0.12, removeNode = T)

## ---- fig.width=6, fig.height=6, fig.align='center'---------------------------
viviNetwork(mat = rf_fit, 
            layout = cbind(c(1,1,1,1,2,2,2,2,2), c(1,2,4,5,1,2,3,4,5)))

## ---- fig.width=6, fig.height=6, fig.align='center'---------------------------
set.seed(1701)
viviNetwork(mat = rf_fit, cluster = igraph::cluster_fast_greedy)

## ---- fig.width=6, fig.height=6, fig.align='center'---------------------------
set.seed(1701)
pdpPairs(data = myData, fit = rf, response = "y", nmax = 50, gridSize = 10)

## ---- fig.width=6, fig.height=6, fig.align='center'---------------------------
set.seed(1701)
pdpPairs(data = myData, fit =  rf, response = "y", nmax = 50, gridSize = 10, 
         vars = c("x1", "x2", "x3", "x4", "x5"))

## ---- fig.width=6, fig.height=6, fig.align='center'---------------------------
set.seed(1701)
pdpZen(data = myData, fit = rf, response = "y", nmax = 50, gridSize = 10)

## ---- fig.width=6, fig.height=6, fig.align='center'---------------------------
set.seed(1701)
zpath <- zPath(viv = rf_fit, cutoff = 0.1)
pdpZen(data = myData, fit = rf, response = "y", nmax = 50, gridSize = 10, zpath = zpath)

## -----------------------------------------------------------------------------
set.seed(1701)
rfClassif <- ranger(Species~ ., data = iris, probability = T, 
                    importance = "impurity")

set.seed(101)
viviClassif  <- vivi(fit = rfClassif, 
                data = iris, 
                response = "Species",
                gridSize = 10,
                importanceType = NULL,
                nmax = 50,
                reorder = TRUE,
                class = "setosa",
                predictFun = NULL)


## ---- fig.width=6, fig.height=6, fig.align='center'---------------------------
set.seed(1701)
viviHeatmap(mat = viviClassif)

## ---- fig.width=6, fig.height=6, fig.align='center'---------------------------
set.seed(1701)
viviNetwork(mat = viviClassif)

## ---- fig.width=6, fig.height=6, fig.align='center'---------------------------
set.seed(1701)
pdpPairs(data = iris, fit = rfClassif, response = "Species", class = "setosa",  convexHull = T, gridSize = 10, nmax = 50) 

## ---- fig.width=6, fig.height=6, fig.align='center'---------------------------
set.seed(1701)
pdpZen(data = iris, fit = rfClassif, response = "Species", class = "setosa",  convexHull = T, gridSize = 10, nmax = 50) 

