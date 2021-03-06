% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/BondCashFlow.R
\name{Bond.Cash.Flow}
\alias{Bond.Cash.Flow}
\title{Bond Cash Flow}
\usage{
Bond.Cash.Flow(issue.date = "character", start.date = "character",
  end.date = "character", coupon = numeric(), principal = numeric(),
  frequency = numeric(), price = numeric())
}
\arguments{
\item{issue.date}{A character string the issue date}

\item{start.date}{A character string the start or dated date}

\item{end.date}{A character string the bond's maturity}

\item{coupon}{A numeric the coupon of the bond}

\item{principal}{A numeric value the principal of the bond}

\item{frequency}{A numeric value the payment frequency}

\item{price}{A numeric value the price of the bond}
}
\description{
Cash flow analysis of a standard non-callable bond
This function is taken from BondLab and is copyright BondLab Technologies, Inc.
see BondLab documentation BondAnalytics
}
\examples{
Bond.Cash.Flow(issue.date = "01-01-2013", start.date = "01-15-2013",
end.date = "01-15-2023", coupon = .05, principal =1000, frequency = 2, price = 100)
}

