% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AKSProvider-adaptor.R
\name{dockerClusterExists,AKSProvider-method}
\alias{dockerClusterExists,AKSProvider-method}
\title{Whether the cluster exists}
\usage{
\S4method{dockerClusterExists}{AKSProvider}(provider, cluster, verbose)
}
\arguments{
\item{provider}{S4 \code{CloudProvider} object. The service provider.}

\item{cluster}{S4 \code{DockerCluster} object.}

\item{verbose}{Integer. The verbose level, default 1.}
}
\value{
logical(1)
}
\description{
This function checks whether the cluster with the same job queue name
exists on the cloud.
}
