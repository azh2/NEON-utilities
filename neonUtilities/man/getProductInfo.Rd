% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getProductInfo.R
\name{getProductInfo}
\alias{getProductInfo}
\title{Get NEON data product information}
\usage{
getProductInfo(dpID)
}
\arguments{
\item{dpID}{The data product id (optional), formated as DP#.#####.###}
}
\value{
A data table with product information
}
\description{
Use the NEON API to get data product information such as availability, science team, etc.
}
\examples{
# Get documentation and availability of plant foliar properties data product
cfcInfo <- getProductInfo("DP1.10026.001")
}
\references{
License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
}
\author{
Christine Laney \email{claney@battelleecology.org}
}
