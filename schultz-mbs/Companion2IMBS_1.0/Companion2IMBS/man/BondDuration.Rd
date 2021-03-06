% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Bondprice.R
\name{BondDuration}
\alias{BondDuration}
\title{Bond duration is a wrapper around Bond.Cash.Flow}
\usage{
BondDuration(issue.date = character, start.date = character,
  end.date = character, coupon = numeric(), frequency = numeric(),
  price = numeric())
}
\arguments{
\item{issue.date}{a character string the issue date}

\item{start.date}{a character string the start date}

\item{end.date}{a character string the end date}

\item{coupon}{a numeric value the coupon 4.0\% is entered as 0.04}

\item{frequency}{a numeric value the number of payments annually}

\item{price}{a numeric value the bond price}
}
\description{
The function returns bond duration as calculated from
the bond cash flow table
}
\examples{
BondDuration(issue.date = "01-10-2013",
start.date = "01-15-2013", end.date = "01-15-2023", coupon = .05, frequency = 2, price = 100)
}

