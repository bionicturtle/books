
  #' This function is used in the computation of the  annulized return of a bond
  #' once the return cashflow matrix is computed
  #' 
  #' @param BeginValue the beginning value of the portfolio position
  #' @param EndValue the ending value of the portfolio position including coupon income, reinvestment, and price change
  #' @param TimeHorizon A numeric value the time horizon over which to calculate the return
  #' @param Frequency the payment frequency of the bond
  #' @export
  AnnualizedReturn <- function(BeginValue = numeric(), 
                               EndValue = numeric(),
                               TimeHorizon = numeric(),
                               Frequency = numeric()){
    
                  FutureValueFactor <- (EndValue/BeginValue) ^ (1/(TimeHorizon * Frequency))
                  FutureValueFactor <- (FutureValueFactor - 1) * 2
                  return(FutureValueFactor)
  }