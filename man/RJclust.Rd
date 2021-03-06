% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AlgorithmImplementation.R
\name{RJclust}
\alias{RJclust}
\title{RJclust}
\usage{
RJclust(X, num_cut = NULL, seed = 1)
}
\arguments{
\item{X}{Data input}

\item{num_cut}{Number of cuts for RJ algorithm (suggested sqrt(p))}

\item{seed}{Seed (defalt = 1)}
}
\value{
Returns RJ algorithm result
}
\description{
RJclust
}
\examples{
X = generateSimulationData()
X = X$X
clust = RJclust(X)
}
