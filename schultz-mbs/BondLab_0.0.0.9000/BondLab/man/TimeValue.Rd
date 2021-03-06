% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/TimeValueFunction.R
\name{TimeValue}
\alias{TimeValue}
\title{A function to compute the time value of money}
\usage{
TimeValue(interest.rate = numeric(), number.periods = numeric(),
  frequency = numeric(), type = "character")
}
\arguments{
\item{interest.rate}{The interest rate used to determine the discount factor in decimal
form (i.e. 4.0\% is input as 0.04)}

\item{number.periods}{The number of discount periods.  For example,
in the case of a mortgage whose cash flow occurs monthly over 30 years the number of
periods is 360.}

\item{frequency}{The frequency of interest payments.  For example the frequency of a
mortgage whose payments occur monthly is 12}

\item{type}{The type of calculation PV = present value,
PVA = present value of annuity, PVAD present value of annuity due,
FV = future value, FVA = future value of annuity.}
}
\description{
A standard generic function used to calculate time value of money
}
\examples{
TimeValue(interest.rate = .05, number.periods = 3, frequency = 1, type = "PV")
}

