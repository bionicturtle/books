#' Present Value 
#' 
#' The function below is a wrapper around the BondLab function TimeValue returning present value
#' @importFrom BondLab TimeValue
#' @param interest.rate a numeric value the discount rate used entered for example 4.0\% is entered as .04
#' @param number.period an numeric value the number of periods over which to discount
#' @param frequency a numeric value the frequency of payments made annually
#' @examples PresentValue(interest.rate = .05, number.period = 3, frequency = 1)
#' @export 
PresentValue <- function(interest.rate = numeric(), 
                         number.period = numeric(), 
                         frequency = numeric()) {
  PV <- TimeValue(interest.rate = interest.rate,
                  number.periods = number.period,
                  frequency = frequency,
                  type = "PV")
  return(PV)
}

#' Future Value 
#' 
#' The function below is a wrapper around the BondLab function TimeValue returning present value
#' @importFrom BondLab TimeValue
#' @param interest.rate a numeric value the discount rate used entered for example 4.0\% is entered as .04
#' @param number.period an numeric value the number of periods in years
#' @param frequency a numeric value the frequency of payments made annually
#' @examples FutureValue(interest.rate = .05, number.period = 3, frequency = 1)
#' @export 
FutureValue <- function(interest.rate = numeric(), 
                         number.period = numeric(), 
                         frequency = numeric()) {
  FV <- TimeValue(interest.rate = interest.rate,
                  number.periods = number.period,
                  frequency = frequency,
                  type = "FV")
  return(FV)
}