#' pdpPairs
#'
#' @description Creates a pairs plot showing bivariate pdp on upper diagonal, ice/univariate pdp on the diagonal and data on the lower diagonal
#'
#' @param data Data frame used for fit.
#' @param fit A supervised machine learning model, which understands condvis2::CVpredict
#' @param response The name of the response for the fit.
#' @param vars The variables to plot (and their order), defaults to all variables other than response.
#' @param pal A vector of colors to show predictions, for use with scale_fill_gradientn
#' @param fitlims Specifies the fit range for the color map. Options are a numeric vector of length 2,
#'  "pdp" (default), in which cases limits are calculated from the pdp, or "all", when limits are calculated from the observations and pdp.
#'  Predictions outside fitlims are squished on the color scale.
#' @param gridSize The size of the grid for evaluating the predictions.
#' @param nmax Uses sample of nmax data rows for the pdp.  Default is 500. Use all rows if NULL.
#' @param class Category for classification, a factor level, or a number indicating which factor level.
#' @param nIce Number of ice curves to be plotted, defaults to 30.
#' @param colorVar Which variable to colour the predictions by.
#' @param comboImage If TRUE  draws pdp for mixed variable plots as an image, otherwise an interaction plot.
#' @param predictFun Function of (fit, data) to extract numeric predictions from fit. Uses condvis2::CVpredict by default, which works for many fit classes.
#' @param convexHull If TRUE, then the convex hull is computed and any points outside the convex hull are removed.
#' @param probability if TRUE, then returns the partial dependence for classification on the probability scale. If
#' FALSE (default), then the partial dependence is returned on a near logit scale.
#' @return A pairs plot
#'
#' @importFrom condvis2 CVpredict
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr %>%
#' @importFrom GGally ggpairs
#' @importFrom GGally eval_data_col
#' @importFrom stats na.omit
#' @importFrom grDevices chull
#' @import ggplot2
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#' # Load in the data:
#' aq <- na.omit(airquality)
#' f <- lm(Ozone ~ ., data = aq)
#' pdpPairs(aq, f, "Ozone")
#' \donttest{
#' # Run a ranger model:
#' library(ranger)
#' library(MASS)
#' Boston1 <- Boston[, c(4:6, 8, 13:14)]
#' Boston1$chas <- factor(Boston1$chas)
#' fit <- ranger(medv ~ ., data = Boston1, importance = "permutation")
#' pdpPairs(Boston1[1:30, ], fit, "medv")
#' pdpPairs(Boston1[1:30, ], fit, "medv", comboImage = TRUE)
#' viv <- vivi(Boston1, fit, "medv")
#' # show top variables only
#' pdpPairs(Boston1[1:30, ], fit, "medv", comboImage = TRUE, vars = rownames(viv)[1:4])
#' }
#' \donttest{
#' library(ranger)
#' rf <- ranger(Species ~ ., data = iris, probability = TRUE)
#' pdpPairs(iris, rf, "Species") # prediction probs for first class, setosa
#' pdpPairs(iris, rf, "Species", class = "versicolor") # prediction probs versicolor
#' }
#' @export




pdpPairs <- function(data,
                      fit,
                      response,
                      vars = NULL,
                      pal = rev(RColorBrewer::brewer.pal(11, "RdYlBu")),
                      fitlims = "pdp",
                      gridSize = 10,
                      nmax = 500,
                      class = 1,
                      nIce = 30,
                      colorVar = NULL,
                      comboImage = FALSE,
                      predictFun = NULL,
                      convexHull = FALSE,
                      probability = FALSE) {
  data <- na.omit(data)
  if (is.null(nmax)) nmax <- nrow(data)
  nmax <- max(5, nmax)
  if (is.numeric(nmax) && nmax < nrow(data)) {
    data <- data[sample(nrow(data), nmax), , drop = FALSE]
  }
  gridSize <- min(gridSize, nmax)

  classif <- is.factor(data[[response]]) | inherits(fit, "LearnerClassif")

  if (classif) {
    if (probability) {
      legendName <- "y-hat\nprobability"
    } else {
      legendName <- "y-hat\nlogit"
    }
  } else {
    legendName <- "y-hat"
  }


  if (is.null(predictFun)) predictFun <- CVpredictfun(classif, class)

  if (classif) {
    predData <- predictFun(fit, data, prob = probability)
  } else {
    predData <- predictFun(fit, data)
  }


  vars0 <- names(data)
  vars0 <- vars0[-match(response, vars0)]
  vars <- vars[vars %in% vars0]
  if (is.null(vars)) vars <- vars0

  if (length(nIce) > 1) {
    nIce <- nIce[nIce <= nrow(data)]
    sice <- c(NA, nIce)
  } else {
    nIce <- min(nIce, nrow(data))
    sice <- c(NA, sample(nrow(data), nIce)) # for use with iceplots
  }

  # loop through vars and create a list of pdps

  message("Generating ice/pdp fits... waiting...")

  data$predData <- predData
  pdplist1 <- vector("list", length = length(vars))
  for (i in 1:length(vars)) {
    px <- pdp_data(data, vars[i], gridsize = gridSize)
    px$.pid <- i
    pdplist1[[i]] <- px
  }
  pdplist1 <- bind_rows(pdplist1)
  if (classif) {
    pdplist1$fit <- predictFun(fit, pdplist1, prob = probability)
  } else {
    # only for use with keras models
    if (any(sapply(class(fit), function(x) grepl("keras", x)))) {
      numberCol <- ncol(pdplist1)
      pdplist1Keras <- pdplist1[,-c(numberCol-2, numberCol-1, numberCol)]
      pdplist1$fit <- predictFun(fit, pdplist1Keras) # had to explicitly remove the extra colmns here
    } else {
      pdplist1$fit <- predictFun(fit, pdplist1[, 1:(ncol(pdplist1) - 3)])
    }
    #pdplist1$fit <- predictFun(fit, pdplist1)
  }


  pdplist1 <- split(pdplist1, pdplist1$.pid)

  names(pdplist1) <- vars

  # Get names for pairs of variables

  xyvar <- expand.grid(1:length(vars), 1:length(vars))[, 2:1]
  xyvar <- as.matrix(xyvar[xyvar[, 1] < xyvar[, 2], ])
  xyvarn <- cbind(vars[xyvar[, 1]], vars[xyvar[, 2]])


  # loop through vars and create a list of pdps for each pair

  pdplist <- vector("list", length = nrow(xyvarn))
  for (i in 1:nrow(xyvarn)) {
    px <- pdp_data(data, xyvarn[i, ], gridsize = gridSize, convexHull = convexHull)
    px$.pid <- i
    pdplist[[i]] <- px
  }

  pdplist <- bind_rows(pdplist)

  if (classif) {
    pdplist$fit <- predictFun(fit, pdplist, prob = probability)
  } else {
    # only for use with keras models
    if (any(sapply(class(fit), function(x) grepl("keras", x)))) {
      numberColumn <- ncol(pdplist)
      pdplistKeras <- pdplist[,-c(numberColumn-2, numberColumn-1, numberColumn)]
      pdplist$fit <- predictFun(fit, pdplistKeras) # had to explicitly remove the extra colmns here
    } else {
      pdplist$fit <- predictFun(fit, pdplist[, 1:(ncol(pdplist) - 3)])
    }
    #pdplist$fit <- predictFun(fit, pdplist)
  }
  pdplist <- split(pdplist, pdplist$.pid)

  for (i in 1:nrow(xyvarn)) {
    pdplist[[i]] <- pdplist[[i]] %>%
      group_by(.data[[xyvarn[i, 1]]], .data[[xyvarn[i, 2]]]) %>%
      summarise(fit = mean(fit))
  }
  names(pdplist) <- paste(xyvarn[, 2], xyvarn[, 1], sep = "pp")

  message("Finished ice/pdp")

  # Set limits for pairs
  if (fitlims[1] == "all") {
    r <- sapply(pdplist, function(x) range(x$fit))
    r <- range(c(r, predData))
    limits <- range(labeling::rpretty(r[1], r[2]))
  } else if (fitlims[1] == "pdp") {
    r <- sapply(pdplist, function(x) range(x$fit))
    r <- range(r)
    limits <- range(labeling::rpretty(r[1], r[2]))
  } else {
    limits <- fitlims
  }


  pdpnn <- function(data, mapping, ...) {
    vars <- c(quo_name(mapping$x), quo_name(mapping$y))
    pdp <- pdplist[[paste(vars[1], vars[2], sep = "pp")]]
    ggplot(data = pdp, aes(x = .data[[vars[1]]], y = .data[[vars[2]]])) +
      geom_tile(aes(fill = fit)) +
      scale_fill_gradientn(name = legendName, colors = pal, limits = limits, oob = scales::squish)
  }

  pdpc <- function(data, mapping, ...) {
    vars <- c(quo_name(mapping$x), quo_name(mapping$y))
    pdp <- pdplist[[paste(vars[1], vars[2], sep = "pp")]]
    if (is.factor(pdp[[vars[1]]])) vars <- rev(vars)
    ggplot(data = pdp, aes(x = .data[[vars[1]]], y = fit, color = .data[[vars[2]]])) +
      geom_line()
  }

  ice <- function(data, mapping, ...) {
    var <- quo_name(mapping$x)
    pdp <- pdplist1[[var]]
    aggr <- pdp %>%
      group_by(.data[[var]]) %>%
      summarise(fit = mean(fit))


    if (is.null(colorVar)) {
      filter(pdp, .data[[".id"]] %in% sice) %>%
        ggplot(aes(x = .data[[var]], y = fit)) +
        geom_line(aes(color = predData, group = .data[[".id"]])) +
        scale_color_gradientn(
          name = legendName, colors = pal, limits = limits, oob = scales::squish,
          guide = guide_colorbar(
            frame.colour = "black",
            ticks.colour = "black"
          )
        ) +
        geom_line(data = aggr,# size = 1,
                  linewidth = 1, color = "black", lineend = "round", group = 1)
    } else {
      filter(pdp, .data[[".id"]] %in% sice) %>%
        ggplot(aes(x = .data[[var]], y = fit)) +
        geom_line(aes(color = .data[[colorVar]], group = .data[[".id"]])) +
        geom_line(data = aggr, #size = 1,
                  linewidth = 1, color = "black", lineend = "round", group = 1)
    }
  }




  rep <- data[[response]]
  dplotn <- function(data, mapping) {
    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)

    if (is.null(colorVar)) {
      df <- data.frame(x = x, y = y)
      df %>%
        ggplot(aes(x = .data$x, y = .data$y, color = predData)) +
        geom_point(shape = 16, size = 1, show.legend = FALSE) +
        scale_colour_gradientn(name = legendName, colors = pal, limits = limits, oob = scales::squish)
    } else {
      data$response <- rep
      names(data)[names(data) == "response"] <- response
      colorVar <- data[[colorVar]]
      df <- data.frame(x = x, y = y, colVar = colorVar)
      df %>%
        ggplot(aes(x = .data$x, y = .data$y)) +
        geom_point(shape = 16, size = 1, show.legend = FALSE, aes(color = .data$colVar))
    }
  }

  dplotm <- function(data, mapping) {
    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)
    df <- data.frame(x = x, y = y)
    jitterx <- if (is.factor(df$x)) .25 else 0
    jittery <- if (is.factor(df$y)) .25 else 0


    ggplot(df, aes(x = x, y = y, color = predData)) +
      geom_jitter(shape = 16, size = 1, show.legend = FALSE, width = jitterx, height = jittery) +
      scale_colour_gradientn(name = legendName, colors = pal, limits = limits, oob = scales::squish)
  }


  wlegend <- 1

  p <- ggpairs(data[vars],
               upper = list(continuous = pdpnn, combo = if (comboImage) pdpnn else pdpc, discrete = pdpnn),
               diag = list(continuous = ice, discrete = ice),
               lower = list(continuous = dplotn, combo = dplotm, discrete = dplotm),
               legend = wlegend,
               cardinality_threshold = NULL
  ) +
    theme_bw() +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      axis.line = element_line(),
      # axis.ticks = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 1, size = 6),
      axis.text.y = element_text(size = 6),
      strip.text = element_text(face = "bold", colour = "black", size = 7)
    )


  suppressMessages(print(p))
  invisible(p)
}




pdp_data <- function(d, var, gridsize = 30, convexHull = FALSE) {
  if (length(var) == 1) {
    pdpvar <- d[[var]]
    if (is.factor(pdpvar)) {
      gridvals <- levels(pdpvar)
    } else {
      gridvals <- seq(min(pdpvar, na.rm = T), max(pdpvar, na.rm = T), length.out = gridsize)
    }
    dnew <- do.call(rbind, lapply(gridvals, function(i) {
      d1 <- d
      d1[[var]] <- i
      d1
    }))
    if (is.factor(pdpvar)) dnew[[var]] <- factor(dnew[[var]], levels = levels(pdpvar), ordered = is.ordered(pdpvar))
  } else {
    pdpvar1 <- d[[var[1]]]
    pdpvar2 <- d[[var[2]]]

    if (is.factor(pdpvar1)) {
      gridvals1 <- levels(pdpvar1)
    } else {
      gridvals1 <- seq(min(pdpvar1, na.rm = T), max(pdpvar1, na.rm = T), length.out = gridsize)
    }
    if (is.factor(pdpvar2)) {
      gridvals2 <- levels(pdpvar2)
    } else {
      gridvals2 <- seq(min(pdpvar2, na.rm = T), max(pdpvar2, na.rm = T), length.out = gridsize)
    }
    gridvals <- expand.grid(gridvals1, gridvals2)

    if (convexHull) {
      if (is.factor(pdpvar1) && is.factor(pdpvar2)) {
        t <- table(pdpvar1, pdpvar2)
        w <- sapply(1:nrow(gridvals), function(i) t[gridvals[i, 1], gridvals[i, 2]] == 0)
        gridvals <- gridvals[!w, ]
      } else if (is.factor(pdpvar1) && is.numeric(pdpvar2)) {
        rangeData <- tapply(pdpvar2, pdpvar1, range)
        w <- sapply(1:nrow(gridvals), function(i) {
          r <- rangeData[[as.character(gridvals[i, 1])]]
          gridvals[i, 2] >= r[1] && gridvals[i, 2] <= r[2]
        })

        gridvals <- gridvals[w, ]
      } else if (is.numeric(pdpvar1) && is.factor(pdpvar2)) {
        rangeData <- tapply(pdpvar1, pdpvar2, range)
        w <- sapply(1:nrow(gridvals), function(i) {
          r <- rangeData[[as.character(gridvals[i, 2])]]
          gridvals[i, 1] >= r[1] && gridvals[i, 1] <= r[2]
        })

        gridvals <- gridvals[w, ]
      } else {
        hpts <- chull(pdpvar1, pdpvar2) # calc CHull
        hpts <- c(hpts, hpts[1]) # close polygon
        pdpvar1CH <- pdpvar1[hpts] # get x-coords of polygon
        pdpvar2CH <- pdpvar2[hpts] # get y-coords of polygon

        # find which are outside convex hull
        res <- sp::point.in.polygon(gridvals$Var1, gridvals$Var2, pdpvar1CH, pdpvar2CH) != 0

        # remove points outside convex hull
        gridvals <- gridvals[res, ]
      }
    }

    dnew <- do.call(rbind, lapply(1:nrow(gridvals), function(i) {
      d1 <- d
      d1[[var[1]]] <- gridvals[i, 1]
      d1[[var[2]]] <- gridvals[i, 2]
      d1
    }))
    if (is.factor(pdpvar1)) dnew[[var[1]]] <- factor(dnew[[var[1]]], levels = levels(pdpvar1), ordered = is.ordered(pdpvar1))
    if (is.factor(pdpvar2)) dnew[[var[2]]] <- factor(dnew[[var[2]]], levels = levels(pdpvar2), ordered = is.ordered(pdpvar2))
  }
  # making sure the repeats is consistent
  n_repeats <- nrow(dnew) / nrow(d)
  dnew$.id <- rep(1:nrow(d), times = ceiling(n_repeats))

  # dnew$.id <- 1:nrow(d)
  rownames(dnew) <- NULL
  dnew
}
