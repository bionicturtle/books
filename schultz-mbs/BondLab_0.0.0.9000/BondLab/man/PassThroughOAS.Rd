% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/PassThroughOAS.R
\name{PassThroughOAS}
\alias{PassThroughOAS}
\title{The constructor function for the passthrough OAS object}
\usage{
PassThroughOAS(bond.id = "character", trade.date = "character",
  settlement.date = "character", original.bal = numeric(),
  price = numeric(), sigma = numeric(), paths = numeric(),
  PrepaymentAssumption = "character", ..., begin.cpr = numeric(),
  end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric())
}
\arguments{
\item{bond.id}{A character string the MBS cusip number}

\item{trade.date}{A character string the trade date}

\item{settlement.date}{A character string the settlement date}

\item{original.bal}{A numeric value the original balance traded}

\item{price}{A numeric value the trade price}

\item{sigma}{A numeric value interest rate volatility assumption}

\item{paths}{A numeric value the number of simulated paths}

\item{PrepaymentAssumption}{A character string the prepayment assumption either CPR, PPC, or MODEL}

\item{...}{optional values when CPR or PPC prepayment assumption is input}

\item{begin.cpr}{Optional, A numeric value the beginning value of the PPC ramp}

\item{end.cpr}{Optional, A numeric value the end value of the PPC ramp}

\item{seasoning.period}{Optional, A numeric value the length of the seasoning ramp}

\item{CPR}{Optional, the assumed CPR}
}
\description{
This function is the Pass Through OAS constructor function
}
\examples{
PassThroughOAS(bond.id = "bondlabMBS4", trade.date = "01-10-2013", settlement.date = "01-13-2013",
original.bal = 100000, price = 105.75, sigma = 0.015, paths = 200, PrepaymentAssumption = "MODEL")
}

