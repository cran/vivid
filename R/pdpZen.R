#' Create a zenplot displaying partial dependence values.
#'
#' @description Constructs a zigzag expanded navigation plot (zenplot) displaying partial dependence values.
#'
#' @param data Data frame used for fit
#' @param fit A supervised machine learning model, which understands condvis2::CVpredict
#' @param response The name of the response for the fit
#' @param zpath Plot shows consecutive pairs of these variables. Defaults to all variables other than response.
#' Recommend constructing zpath witn \code{calcZpath}.
#' @param pal A vector of colors to show predictions, for use with scale_fill_gradientn
#' @param fitlims Specifies the fit range for the color map. Options are a numeric vector of length 2,
#'  "pdp" (default), in which cases limits are calculated from the pdp, or "all", when limits are calculated from the observations and pdp
#'  predictions outside fitlims are squished on the color scale.
#' @param gridSize The size of the grid for evaluating the predictions.
#' @param nmax Uses sample of nmax data rows for the pdp.  Default is 500. Use all rows if NULL.
#' @param class Category for classification, a factor level, or a number indicating which factor level.
#' @param comboImage If TRUE  draws pdp for mixed variable plots as an image, otherwise an interaction plot.
#' @param rug If TRUE adds rugs for the data to the pdp plots
#' @param predictFun Function of (fit, data) to extract numeric predictions from fit. Uses condvis2::CVpredict by default, which works for many fit classes.
#' @param convexHull If TRUE, then the convex hull is computed and any points outside the convex hull are removed.
#' @param probability if TRUE, then returns the partial dependence for classification on the probability scale. If
#' FALSE (default), then the partial dependence is returned on a near logit scale.
#' @param ... passed on to zenplot
#' @return A zenplot of partial dependence values.
#'
#' @importFrom stats na.omit
#' @importFrom RColorBrewer brewer.pal
#' @examples
#' \dontrun{
#' # To use this function, install zenplots and graph from Bioconductor.
#' if (!requireNamespace("graph", quietly = TRUE)) {
#'   install.packages("BiocManager")
#'   BiocManager::install("graph")
#' }
#' install.packages("zenplots")
#'
#' library(MASS)
#' library(ranger)
#' Boston1 <- Boston
#' Boston1$chas <- factor(Boston1$chas)
#' rf <- ranger(medv ~ ., data = Boston1)
#' pdpZen(Boston1[1:30, ], rf, response = "medv", zpath = names(Boston1)[1:4], comboImage = T)
#' # Find the top variables in rf
#' set.seed(123)
#' viv <- vivi(Boston1, rf, "medv", nmax = 30) # use 30 rows, for speed
#' pdpZen(Boston1, rf, response = "medv", zpath = rownames(viv)[1:4], comboImage = T)
#' zpath <- zPath(viv, cutoff = .2) # find plots whose interaction score exceeds .2
#' pdpZen(Boston1, rf, response = "medv", zpath = zpath, comboImage = T)
#' }
#' @export



pdpZen <- function(data,
                   fit,
                   response,
                   zpath = NULL,
                   pal = rev(RColorBrewer::brewer.pal(11, "RdYlBu")),
                   fitlims = "pdp",
                   gridSize = 10,
                   nmax = 500,
                   class = 1,
                   comboImage = FALSE,
                   rug = TRUE,
                   predictFun = NULL,
                   convexHull = FALSE,
                   probability = FALSE,
                   ...) {
  if (!(requireNamespace("zenplots", quietly = TRUE))) {
    message("Please install package zenplots to use this function. Note zenplots requires packge graph from Bioconductor, see help for zenplots for instructions.")
    return(invisible(NULL))
  }

  data <- na.omit(data)
  if (is.null(nmax)) nmax <- nrow(data)
  nmax <- max(5, nmax)
  if (is.numeric(nmax) && nmax < nrow(data)) {
    data <- data[sample(nrow(data), nmax), , drop = FALSE]
  }
  gridSize <- min(gridSize, nmax)

  classif <- is.factor(data[[response]]) | inherits(fit, "LearnerClassif")


  if (is.null(predictFun)) predictFun <- CVpredictfun(classif, class)

  if (classif) {
    predData <- predictFun(fit, data, prob = probability)
  } else {
    predData <- predictFun(fit, data)
  }



  if (!classif && probability) {
    warning("Probability scale is for classification only and will be ignored")
  }



  vars <- names(data)
  vars <- vars[-match(response, vars)]
  datap <- data[, vars]

  if (is.null(zpath)) {
    zpath <- 1:length(vars)
    zdata <- datap
    zpairs <- t(sapply(1:(length(zpath) - 1), function(i) {
      z <- zpath[i:(i + 1)]
      if (i %% 2 == 0) rev(z) else z
    }))
  } else if (is.character(zpath)) {
    zpath <- match(zpath, vars)
    if (any(is.na(zpath))) stop("'zpath' should contain predictor names.")
    zdata <- zenplots::indexData(datap, zpath)
    zpairs <- t(sapply(1:(length(zpath) - 1), function(i) {
      z <- zpath[i:(i + 1)]
      if (i %% 2 == 0) rev(z) else z
    }))
  } else if (is.list(zpath)) {
    zpath0 <- unlist(zpath)
    zpath0 <- match(zpath0, vars)
    if (any(is.na(zpath0))) stop("'zpath' should contain predictor names.")
    zpath <- lapply(zpath, function(z) match(z, vars))
    zpairs <- t(sapply(1:(length(zpath0) - 1), function(i) {
      z <- zpath0[i:(i + 1)]
      if (i %% 2 == 0) rev(z) else z
    }))
    fixind <- cumsum(sapply(zpath, length))
    fixind <- fixind[-length(fixind)]
    for (i in fixind) zpairs[i, ] <- NA
    zdata <- zenplots::groupData(datap, indices = zpath)
  }

  zpairs <- cbind(vars[zpairs[, 1]], vars[zpairs[, 2]])



  # loop through vars and create a list of pdps for each pair
  pdplist <- vector("list", nrow(zpairs))
  message("Generating ice/pdp fits... waiting...")


  for (i in 1:nrow(zpairs)) {
    ind <- zpairs[i, ]
    if (!is.na(ind[1])) {
      px <- pdp_data(data, ind, gridsize = gridSize, convexHull = convexHull)
      px$.pid <- i
      pdplist[[i]] <- px
    } else {
      pdplist[[i]] <- NULL
    }
  }

  pdplist <- bind_rows(pdplist)
  if (classif) {
    pdplist$fit <- predictFun(fit, pdplist, prob = probability)
  } else {
    # only for use with keras models
    if (any(sapply(class(fit), function(x) grepl("keras", x)))) {
      numberCol <- ncol(pdplist)
      pdplistKeras <- pdplist[,-c(numberCol-1, numberCol)]
      pdplist$fit <- predictFun(fit, pdplistKeras) # had to explicitly remove the extra colmns here
    } else {
      pdplist$fit <- predictFun(fit, pdplist[, 1:(ncol(pdplist) - 2)])
    }
    #pdplist$fit <- predictFun(fit, pdplist)
  }
  pdplist <- split(pdplist, pdplist$.pid)

  pdplist0 <- vector("list", nrow(zpairs))

  j <- 1
  for (i in 1:nrow(zpairs)) {
    ind <- zpairs[i, ]
    if (!is.na(ind[1])) {
      pdplist0[[i]] <- pdplist[[j]] %>%
        group_by(.data[[ind[1]]], .data[[ind[2]]]) %>%
        summarise(fit = mean(fit))
      j <- j + 1
    } else {
      pdplist0[[i]] <- NULL
    }
  }

  pdplist <- pdplist0
  pdplist0 <- NULL
  names(pdplist) <- paste(zpairs[, 2], zpairs[, 1], sep = "pp")
  message("Finished ice/pdp")

  # Set limits for pairs
  if (fitlims[1] == "pdp") {
    pdplist0 <- pdplist[!sapply(pdplist, is.null)]
    r <- range(sapply(pdplist0, function(x) range(x$fit)))
    limits <- range(labeling::rpretty(r[1], r[2]))
  } else if (fitlims[1] == "all") {
    pdplist0 <- pdplist[!sapply(pdplist, is.null)]
    r <- range(sapply(pdplist0, function(x) range(x$fit)))
    r <- range(c(r, predData))
    limits <- range(labeling::rpretty(r[1], r[2]))
  } else {
    limits <- fitlims
  }


  # Zenplot graphing function
  data$pred <- predData
  z2index <- 0
  pdpnn <- function(zargs) {
    z2index <<- z2index + 1
    vars <- zpairs[z2index, ]
    pdp <- pdplist[[z2index]]
    if (!is.null(pdp)) {
      if (!comboImage && is.factor(pdp[[vars[1]]]) + is.factor(pdp[[vars[2]]]) == 1) {
        flip <- is.factor(pdp[[vars[1]]])
        if (flip) vars <- rev(vars)

        p <- ggplot(data = pdp, aes(x = .data[[vars[1]]], y = fit, color = .data[[vars[2]]])) +
          geom_line() +
          geom_rug(data = data, sides = "b", aes(y = .data[["pred"]]), color = "black", alpha = 0.1)
      } else {
        if (is.factor(pdp[[vars[1]]])) posx <- "jitter" else posx <- "identity"
        if (is.factor(pdp[[vars[2]]])) posy <- "jitter" else posy <- "identity"

        num2d <- zargs$num / 2
        p <- ggplot(data = pdp, aes(x = .data[[vars[1]]], y = .data[[vars[2]]])) +
          geom_tile(aes(fill = fit)) +
          scale_fill_gradientn(name = "y-hat", colors = pal, limits = limits, oob = scales::squish)
        if (rug) {
          p <- p +
            geom_rug(data = data, sides = "b", position = posx, alpha = 0.1, color = "black") +
            geom_rug(data = data, sides = "l", position = posy, alpha = 0.1, color = "black")
        }
      }

      p <- p +
        guides(fill = "none", color = "none") +
        theme_bw() +
        theme(
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_rect(colour = "gray", fill = NA, size = 1.5)
        )
    } else {
      p <- ggplot() +
        theme(panel.background = element_blank())
    }

    ggplot_gtable(ggplot_build(p))
  }

  suppressMessages({
    zenplots::zenplot(zdata,
      pkg = "grid", labs = list(group = NULL),
      plot2d = pdpnn,
      ...
    )
  })
}
