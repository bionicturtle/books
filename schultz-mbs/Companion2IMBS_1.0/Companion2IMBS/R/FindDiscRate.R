#' FindDiscRate
#' 
#' Function used with uni root to find discount rate of a set cashflows
#' @param interest.rate  A numeric vlaue the interest rate used to discount
#' @param LumpSumPayment A numeric value the proceeds paid by the investor
#' @param payment A numeric value the payment received
#' @param number.periods A numeric value the number of periods
#' @param frequency of payment 
#' @export
FindDiscRate <- function(interest.rate = numeric(), 
                          LumpSumPayment = numeric(),
                          payment = numeric(),
                          number.periods = numeric(), 
                          frequency = numeric()){
      interest.rate = interest.rate/frequency
      LumpSumPayment - (payment * ((1-(1/(1+interest.rate)^number.periods))/interest.rate))}