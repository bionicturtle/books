% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/FindDiscRate.R
\name{FindDiscRate}
\alias{FindDiscRate}
\title{FindDiscRate}
\usage{
FindDiscRate(interest.rate = numeric(), LumpSumPayment = numeric(),
  payment = numeric(), number.periods = numeric(), frequency = numeric())
}
\arguments{
\item{interest.rate}{A numeric vlaue the interest rate used to discount}

\item{LumpSumPayment}{A numeric value the proceeds paid by the investor}

\item{payment}{A numeric value the payment received}

\item{number.periods}{A numeric value the number of periods}

\item{frequency}{of payment}
}
\description{
Function used with uni root to find discount rate of a set cashflows
}

