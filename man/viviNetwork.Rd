% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/viviNetwork.R
\name{viviNetwork}
\alias{viviNetwork}
\title{viviNetwork}
\usage{
viviNetwork(
  mat,
  intThreshold = NULL,
  intLims = NULL,
  impLims = NULL,
  intPal = rev(colorspace::sequential_hcl(palette = "Purples 3", n = 100)),
  impPal = rev(colorspace::sequential_hcl(palette = "Greens 3", n = 100)),
  removeNode = FALSE,
  layout = igraph::layout_in_circle,
  cluster = NULL,
  nudge_x = 0.05,
  nudge_y = 0.03,
  edgeWidths = 1:4
)
}
\arguments{
\item{mat}{A matrix, such as that returned by vivi, of values to be plotted.}

\item{intThreshold}{Remove edges with weight below this value if provided.}

\item{intLims}{Specifies the fit range for the color map for interaction strength.}

\item{impLims}{Specifies the fit range for the color map for importance.}

\item{intPal}{A vector of colours to show interactions, for use with scale_fill_gradientn.}

\item{impPal}{A vector of colours to show importance, for use with scale_fill_gradientn.}

\item{removeNode}{If TRUE, then removes nodes with no connecting edges when thresholding interaction values.}

\item{layout}{igraph layout function or a numeric matrix with two columns, one row per node. Defaults to igraph::layout_as_circle}

\item{cluster}{Either a vector of cluster memberships for nodes or an igraph clustering function.}

\item{nudge_x}{Nudge (centered) labels by this amount, outward horizontally.}

\item{nudge_y}{Nudge (centered) labels by this amount, outward vertically.}

\item{edgeWidths}{A vector specifying the scaling of the edges for the displayed graph. Values must be positive.}
}
\value{
A plot displaying interaction strength between variables on the edges and variable importance on the nodes.
}
\description{
Create a Network plot displaying variable importance
and variable interaction.
}
\examples{
\donttest{
library(ranger)
aq <- na.omit(airquality)
rF <- ranger(Ozone ~ ., data = aq, importance = "permutation")
myMat <- vivi(fit = rF, data = aq, response = "Ozone")
viviNetwork(myMat)
}
}
